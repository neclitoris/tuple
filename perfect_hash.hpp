#pragma once

#include <algorithm>
#include <cstdint>
#include <iterator>
#include <numeric>
#include <span>
#include <type_traits>
#include <vector>

std::span<const std::byte> data(std::span<const std::byte> u) { return u; }

template <std::integral T>
std::span<const std::byte> data(const T& v) {
    return as_bytes(std::span{&v, 1});
}

template <typename T>
concept Binary = requires {
    std::is_convertible_v<decltype(data(std::declval<T>())),
                          std::span<const std::byte>>;
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
    return hash_fun{base, [](size_t hash, std::span<const std::byte> data) {
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
        std::vector<std::vector<std::span<const std::byte>>> buckets;
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
                      layer2.size()] = data(Keys)));
    }

    constexpr size_t operator[](Binary auto key) const {
        auto idx = layer1[default_hash_fun(key) % layer1.size()](key);
        if (idx == static_cast<size_t>(-1))
            return -1;
        if (!std::ranges::equal(layer2[idx % layer2.size()], data(key)))
            return -1;
        return idx % layer2.size();
    }

  private:
    std::array<hash_fun, sizeof...(Keys)> layer1;
    std::array<std::span<const std::byte>, sizeof...(Keys)> layer2;
};
