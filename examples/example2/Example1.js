exports.runGC = function() {}

exports.usleep = function() { return function() {} }

exports.unsafeCoerce = function(a) { return a }

exports.consoleLog = function(s) {
  return function () {
    console.log(s)
  }
}
