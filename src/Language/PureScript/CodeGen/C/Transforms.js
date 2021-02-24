const jenkins = require('jenkins-hash');

exports.utf8len = function utf8len(s) {
  return Buffer.from(s, 'utf8').length + 1 /* NULL byte for C */;
}

exports.jenkinsHash = function jenkinsHash(s) {
  return jenkins.hashlittle(Buffer.from(s, 'utf8'));
}
