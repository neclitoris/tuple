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

    template <typename Return, typename... Args>
    struct callable_wrapper<Return(Args...)> {
        virtual ~callable_wrapper() = default;
        virtual Return operator()(Args...) = 0;
    };

    template <typename Signature>
    class function_wrapper;

    template <typename Return, typename... Args>
    class function_wrapper<Return(Args...)> : public callable_wrapper<Return(Args...)> {
    public:
        function_wrapper(Return (*f)(Args...)) : func_(f) {}

        Return operator()(Args... args) override { return func_(std::forward<Args>(args)...); }

    private:
        Return (*func_)(Args...) = nullptr;
    };

    template <typename Obj, typename Signature>
    struct object_wrapper;

    template <typename Obj, typename Return, typename... Args>
    struct object_wrapper<Obj, Return(Args...)> : public Obj,
                                                  public callable_wrapper<Return(Args...)> {
        object_wrapper(const Obj& object) : Obj(object) {}
        object_wrapper(Obj&& object) : Obj(std::move(object)) {}

        Return operator()(Args... args) { return Obj::operator()(std::forward<Args>(args)...); }
    };
}  // namespace detail

template <typename Signature>
class function;

template <typename Return, typename... Args>
class function<Return(Args...)> {
public:
    function(Return (*f)(Args...)) : callable_(new detail::function_wrapper<Return(Args...)>(f)) {}

    template <typename Obj>
    function(Obj&& object)
        : callable_(new detail::object_wrapper<Obj, Return(Args...)>(std::forward<Obj>(object))) {}

    Return operator()(Args... args) { return (*callable_)(std::forward<Args>(args)...); }

private:
    std::unique_ptr<detail::callable_wrapper<Return(Args...)>> callable_;
};

template <typename T>
function(T)->function<signature_t<T>>;
