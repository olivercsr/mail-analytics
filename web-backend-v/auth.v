module auth

import regex

const userid_re := regex.regex_opt(r'^[^\s]*[\w]+[^\s]*$') or { panic(err) }

pub fn check_userid(userid string) bool {
  return userid_re.matches_string(userid)
}

