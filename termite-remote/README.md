# Terminfo for Termite
`termite.terminfo` for addressing issues with remote hosts when using Termite for SSH. From [archlinux Wiki](https://wiki.archlinux.org/title/Termite)

```
# export Termite's Terminfo
infocmp > termite.terminfo

# copy to remote host
scp termite.terminfo user@remote-host:~/

# log in to remote host
ssh user@remote-host

# import Terminfo for current user
tic -x termite.terminfo

# may now remove `terminte.terminfo`
