exports.errorCodeImpl = function(Nothing) {
  return function(Just) {
    return function(error) {
      return error.code
        ? Just (error.code)
        : Nothing
    }
  }
}
