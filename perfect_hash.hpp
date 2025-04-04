#pragma once

#include <algorithm>
#include <concepts>
#include <cstdint>
#include <iterator>
#include <numeric>
#include <ranges>
#include <type_traits>
#include <variant>
#include <vector>

constexpr std::vector<std::byte> data(const std::vector<std::byte>& u) {
    return u;
}

template <std::integral T>
constexpr std::vector<std::byte> data(const T& v) {
    std::vector<std::byte> vec;
    for (size_t i = 0; i != sizeof(v); ++i) {
        vec.push_back(static_cast<std::byte>(v >> 8 * i));
    }
    return vec;
}

template <std::ranges::contiguous_range T>
constexpr std::vector<std::byte> data(const T& r) {
    std::vector<std::byte> v;
    for (auto e : r) {
        auto b = data(e);
        std::copy(std::cbegin(b), std::cend(b), std::back_inserter(v));
    }
    return v;
}

template <typename T>
concept Binary = requires {
    std::is_convertible_v<decltype(data(std::declval<T>())),
                          std::vector<std::byte>>;
};

struct hash_fun {
  public:
    constexpr hash_fun()
        : base_(1),
          f_([](size_t, std::span<const std::byte>) -> size_t { return -1; }) {}
    constexpr hash_fun(size_t base,
                       size_t (*f)(size_t, std::span<const std::byte>))
        : base_(base), f_(f) {}

    constexpr auto operator()(const Binary auto& b) const {
        return (*f_)(base_, data(b));
    }

  private:
    size_t base_;
    size_t (*f_)(size_t, std::span<const std::byte>);
};

constexpr auto make_hash_fun(size_t base) {
    return hash_fun{base, [](size_t hash, std::ranges::range auto data) {
                        for (std::byte byte : data) {
                            hash *= 0x00000100000001b3;
                            hash ^= static_cast<size_t>(byte);
                        }
                        return hash;
                    }};
}

constexpr auto default_hash_fun = make_hash_fun(0xcbf29ce484222325);

template <Binary auto... Keys>
class perfect_hash_table {
  public:
    constexpr perfect_hash_table() {
        std::array<bool, sizeof...(Keys)> occupied = {0};
        std::vector<std::vector<std::vector<std::byte>>> buckets;
        buckets.resize(sizeof...(Keys));
        (..., buckets[default_hash_fun(Keys) % buckets.size()].push_back(
                  data(Keys)));
        std::vector<size_t> bucket_idx;
        bucket_idx.resize(buckets.size());
        std::iota(begin(bucket_idx), end(bucket_idx), 0);
        std::sort(begin(bucket_idx), end(bucket_idx), [&](auto l, auto r) {
            return buckets[l].size() > buckets[r].size();
        });

        size_t i = 0;
        for (; i < bucket_idx.size() && buckets[bucket_idx[i]].size() > 1;
             ++i) {
            size_t ind = bucket_idx[i];
            for (uint64_t base = 1;; ++base) {
                auto hash = make_hash_fun(base);
                bool collision = false;
                for (size_t j = 0; j != buckets[ind].size(); ++j) {
                    auto pos = hash(buckets[ind][j]) % sizeof...(Keys);
                    if (occupied[pos]) {
                        collision = true;
                        while (j > 0) {
                            occupied[hash(buckets[ind][--j]) %
                                     sizeof...(Keys)] = false;
                        }
                        break;
                    } else {
                        occupied[pos] = true;
                    }
                }
                if (!collision) {
                    layer1[ind] = hash;
                    break;
                }
            }
        }

        auto it = begin(occupied);
        for (; i < bucket_idx.size() && buckets[bucket_idx[i]].size() == 1;
             ++i) {
            size_t ind = bucket_idx[i];
            it = std::find(it, end(occupied), false);
            size_t free = it - begin(occupied);
            layer1[ind] =
                hash_fun{free, [](size_t free, std::span<const std::byte>) {
                             return free;
                         }};
            *it = true;
        }

        for (; i < bucket_idx.size(); ++i) {
            size_t ind = bucket_idx[i];
            layer1[ind] =
                hash_fun{0, [](size_t, std::span<const std::byte>) -> size_t {
                             return -1;
                         }};
        }

        (..., (layer2[layer1[default_hash_fun(Keys) % layer1.size()](Keys) %
                      layer2.size()] = check_key{Keys}));
    }

    constexpr size_t operator[](Binary auto key) const {
        auto idx = layer1[default_hash_fun(key) % layer1.size()](key);
        if (idx == static_cast<size_t>(-1))
            return -1;
        idx %= layer2.size();
        return layer2[idx](key) ? idx : -1;
    }

  private:
    struct check_key {
        constexpr bool operator()(auto key) const {
            bool equals;
            std::visit(
                [&](auto stored) {
                    if constexpr (requires {
                                      { key == stored } -> std::same_as<bool>;
                                  })
                        equals = key == stored;
                    else
                        equals = false;
                },
                var);

            return equals;
        }

        std::variant<decltype(Keys)...> var;
    };

    std::array<hash_fun, sizeof...(Keys)> layer1;
    std::array<check_key, sizeof...(Keys)> layer2;
};
