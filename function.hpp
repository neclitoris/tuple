#include <memory>
#include <typeinfo>
#include <utility>

template <typename T>
struct signature {
private:
    template <typename Return, typename... Args>
    static auto helper_(Return (T::*)(Args...)) -> Return (*)(Args...);

    template <typename Return, typename... Args>
    static auto helper_(Return (T::*)(Args...) const) -> Return (*)(Args...);

public:
    using type = std::remove_pointer_t<decltype(helper_(&T::operator()))>;
};

template <typename Return, typename... Args>
struct signature<Return(Args...)> {
    using type = Return(Args...);
};

template <typename Return, typename... Args>
struct signature<Return (*)(Args...)> {
    using type = Return(Args...);
};

template <typename T>
using signature_t = typename signature<T>::type;

namespace detail {
    template <typename Signature>
    struct callable_wrapper;

    template <typename Signature>
    struct callable_wrapper_factory;

    template <typename Return, typename... Args>
    struct callable_wrapper_factory<Return(Args...)> {
        template <typename T, typename... R>
        static std::shared_ptr<callable_wrapper<Return(Args...)>> create(T, R...);
    };

    template <typename Signature, typename... Args>
    struct curry_signature;

    template <typename Return>
    struct curry_signature<Return()> {};

    template <typename Return, typename... Args>
    struct curry_signature<Return(Args...)> {
        using type = Return(Args...);
    };

    template <typename Return, typename Arg, typename... RArgs, typename... Args>
    struct curry_signature<Return(Arg, RArgs...), Arg, Args...>
        : public curry_signature<Return(RArgs...), Args...> {};

    template <typename Return, typename... Args>
    struct callable_wrapper<Return(Args...)>
        : public std::enable_shared_from_this<callable_wrapper<Return(Args...)>> {
        virtual ~callable_wrapper() = default;
        virtual Return operator()(Args...) const = 0;
        template <typename... PArgs>
        auto operator()(PArgs&&... args) const {
            using Signature = typename curry_signature<Return(Args...), PArgs...>::type;
            return callable_wrapper_factory<Signature>::create(
                this->shared_from_this(), std::forward<PArgs>(args)...);
        }
    };

    template <typename Obj, typename Signature>
    struct object_wrapper;

    template <typename Obj, typename Return, typename... Args>
    struct object_wrapper<Obj, Return(Args...)> final : public Obj,
                                                        public callable_wrapper<Return(Args...)> {
        object_wrapper(const Obj& object) : Obj(object) {}
        object_wrapper(Obj&& object) : Obj(std::move(object)) {}

        Return operator()(Args... args) const override {
            return ((Obj*) this)->operator()(std::forward<Args>(args)...);
        }
    };

    template <typename Return, typename... Args>
    template <typename T, typename... R>
    std::shared_ptr<callable_wrapper<Return(Args...)>>
    callable_wrapper_factory<Return(Args...)>::create(T t, R... r) {
        auto lambda = [t, r...](Args... args) mutable {
            return (*t)(std::forward<R>(r)..., std::forward<Args>(args)...);
        };
        return std::shared_ptr<callable_wrapper<Return(Args...)>>(
            new object_wrapper<decltype(lambda), Return(Args...)>(std::move(lambda)));
    }

    template <typename Signature>
    class function_wrapper;

    template <typename Return, typename... Args>
    class function_wrapper<Return(Args...)> final : public callable_wrapper<Return(Args...)> {
    public:
        function_wrapper(Return (*f)(Args...)) : func_(f) {}

        Return operator()(Args... args) const override {
            return func_(std::forward<Args>(args)...);
        }

    private:
        Return (*func_)(Args...) = nullptr;
    };
}  // namespace detail

template <typename Signature>
class function;

template <typename Return, typename... Args>
class function<Return(Args...)> {
    template <typename T>
    friend class function;

public:
    function() = default;

    function(Return (*f)(Args...)) : callable_(new detail::function_wrapper<Return(Args...)>(f)) {}

    template <typename Obj>
    function(Obj object)
        : callable_(new detail::object_wrapper<Obj, Return(Args...)>(std::move(object))) {}

    template <typename Obj>
    function(std::shared_ptr<Obj> ptr)
        : callable_(detail::callable_wrapper_factory<Return(Args...)>::create(ptr)) {}

    template <typename... Prefix>
    auto operator()(Prefix&&... p) {
        function<typename detail::curry_signature<Return(Args...), Prefix...>::type> r;
        r.callable_ = callable_->operator()(std::forward<Prefix>(p)...);
        return r;
    }

    Return operator()(Args... args) {
        return (*callable_)(std::forward<Args>(args)...);
    }

private:
    std::shared_ptr<detail::callable_wrapper<Return(Args...)>> callable_;
};

template <typename T>
function(T)->function<signature_t<T>>;
