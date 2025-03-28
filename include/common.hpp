#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <filesystem>

#include <fmt/core.h>

namespace fs = std::filesystem;

using u8 = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using u64 = std::uint64_t;
using usize = std::uintptr_t;

using i8 = std::int8_t;
using i16 = std::int16_t;
using i32 = std::int32_t;
using i64 = std::int64_t;
using isize = std::intptr_t;

using f32 = float;
using f64 = long double;

using string = std::string;
using string_view = std::string_view;

template <typename T> using vec = std::vector<T>;

