exports.newBasicParserImpl = function (optstring, argv, optind) {
  var mod = require('posix-getopt')
  return new mod.BasicParser(optstring, argv, optind)
}

exports.optindImpl = function (parser) {
  return parser.gop_optind
}

exports.argvImpl = function (parser) {
  return parser.gop_argv
}

exports.getoptImpl = function (parser) {
  return parser.getopt()
}
