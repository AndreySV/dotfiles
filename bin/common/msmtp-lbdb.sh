#!/bin/bash
#
# Script must run with Bash so keep above
# send mail with esmtp but send a copy to lbdb-fecthaddr to harvest
# address
#

if true; then
    echo -e \\n \\n MARK \\n \\n >> /tmp/test.log
    echo "$@" >> /tmp/test.log
fi

# harvest mail (stdin) and major to address
# tee the stdin,
# tee splits input into two copies, one to named file, one to stdout
# instead of named file, we redirect to command, and the other copy to
# stdout is piped to a second command
#
# the first command harvests email addresses from the input email
# the second command sends the input e-mail, and takes the parameters
# via “$@” which are the from and to/cc/etc addresses

tee >(lbdb-fetchaddr -a -c utf-8 1> /tmp/fetchaddr.log 2>&1 ) | tee /tmp/test.data | /usr/bin/msmtp -X /tmp/msmtp.log $@
# tee | /usr/bin/msmtp -X /tmp/msmtp.log $@
