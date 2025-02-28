// myplugin

// documentation via: haraka -c /home/oliver/git/mail-analytics/haraka/test01 -h plugins/myplugin

// Put your plugin code here
// type: `haraka -h Plugins` for documentation on how to create a plugin

exports.hook_rcpt = function (next, connection, params) {
  var rcpt = params[0];
  this.loginfo("Got recipient: " + rcpt);
  next();
}
