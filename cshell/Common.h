#ifndef __LLVM_CSHELL_COMMON_H__
#define __LLVM_CSHELL_COMMON_H__

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <variant>
#include <vector>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Error.h>

using namespace std::placeholders;

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...)->overloaded<Ts...>;

/* Just log the error to err stream, to avoid abort of unchecked error.
 *
 * Similar to llvm::logAllUnhandledErrors, but also works for llvm::Expected<T>.
 */
inline bool discardError(llvm::Error &&err, bool log = true, llvm::raw_ostream &OS = llvm::errs()) {
    if (err) {
        llvm::handleAllErrors(std::move(err),
                              [&OS, log](std::unique_ptr<llvm::ErrorInfoBase> payload) {
                                  if (log) {
                                      payload->log(OS);
                                      OS << '\n';
                                  }
                              });
        return false;
    }
    return true;
}

template <typename T>
inline bool discardError(llvm::Expected<T> &val, bool log = true,
                         llvm::raw_ostream &OS = llvm::errs()) {
    return discardError(val.takeError(), log, OS);
}

template <typename T>
inline bool discardError(llvm::Expected<T> &&val, bool log = true,
                         llvm::raw_ostream &OS = llvm::errs()) {
    return discardError(val.takeError(), log, OS);
}

namespace fmt {
/* Extend fmt to StringRef
 */
template <>
struct formatter<llvm::StringRef> {
    template <typename ParseContext>
    constexpr auto parse(ParseContext &Ctx) {
        return Ctx.begin();
    }

    template <typename FormatContext>
    auto format(llvm::StringRef const &Str, FormatContext &Ctx) {
        return format_to(Ctx.begin(), "{}", Str.data());
    }
};
}  // namespace fmt

#endif