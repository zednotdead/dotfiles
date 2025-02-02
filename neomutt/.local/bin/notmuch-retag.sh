notmuch new
notmuch tag +newsletter -- \
  tag:new and \
  "(from:team@egghead.io or \
  from:404-media@ghost.io or \
  from:hello@glow-diaries.com or \
  from:@substack.com or \
  from:@mail.selfh.st)"
notmuch tag -new +inbox -- tag:new

