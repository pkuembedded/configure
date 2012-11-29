
# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'
#alias ls=' ls --color'
alias l='ls -aF'
alias ll='ls -alF'
alias lg='ls -a|grep'
alias llg='ls -alF|grep'
alias lm='ls --color -alF|more'
alias df='df -h'
alias free='free -m'
alias emac='emacs -nw'
alias emacx='emacs'
#alias emacs='emacs -nw'
alias halt='sudo halt'
#alias reboot='sudo reboot'
#alias pacman='sudo pacman'
alias wicd='sudo wicd'
alias cdp='cd ..'
alias cdpp='cd ../..'
alias cdppp='cd ../../..'
#PS1='[\u@\h \W]\$ '

#set JAVA_HOME=/usr/lib/jvm/java-6-openjdk
#export JAVA_HOME
#set PATH=$JAVA_HOME/bin:$PATH
#export PATH
#set CLASSPATH=.:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar
#export CLASSPATH 
#PATH=$PATH:/usr/local/hybus-arm-linux-R1.1/bin


#export PROJECT=arm-dev/toolchain
#export PRJROOT=/home/infinite/$PROJECT
#export TARGET=arm-linux
#export PREFIX=$PRJROOT/tools
#export TARGET_PREFIX=$PREFIX/$TARGET
#export PATH=$PREFIX/bin:$PATH
#cd $PRJROOT
#PATH=$PATH:"/usr/local/hybus-arm-linux-R1.1/bin"
#HOME=/home/infinite
PATH=$PATH:/home/infinite/bin
export QTDIR=/usr/local/Trolltech/Qt-4.6.3
#export QTDIR=/usr/local/Trolltech/QtEmbedded-4.6.3-arm
export PATH=$PATH:$QTDIR/bin
export PATH=$PATH:"/opt/android-sdk-update-manager/tools"
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$QTDIR/lib
#export CVSROOT=/home/infinite/CVS/kernel

#export WINDOW_MANAGER="/usr/bin/awesome"
#export AWT_TOOLKIT=MToolkit
export ANDROID_SDK_HOME="/home/infinite/Project/android"
export PATH=$ANDROID_SDK_HOME:$PATH
#NDKROOT=/opt/android-ndk
#NDKROOT="/opt/android-ndk"
NDKROOT="/home/infinite/android-ndk-r7b"
export NDKROOT
export PATH=$NDKROOT/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/bin:$PATH
export ALTERNATE_EDITOR=""

#C_INCLUDE_PATH=/opt/android-ndk/platforms/android-8/arch-arm/usr/include
#export C_INCLUDE_PATH
#LIBRARY_PATH=/opt/android-ndk/platforms/android-8/arch-arm/usr/lib/
#export LIBRARY_PATH
export HISTFILESIEZ=2000
export HISTCONTROL="erasedups"
extract () {
   if [  -f $1 ] ; then
       case $1 in
	   *.tar.bz2)tar xvjf $1 && cd $(basename "$1" .tar.bz2) ;;
	   *.tar.gz)tar xvzf $1 && cd $(basename "$1" .tar.gz) ;;
	   *.tar.xz)tar Jxvf $1 && cd $(basename "$1" .tar.xz) ;;
	   *.bz2)bunzip2 $1 && cd $(basename "$1" .bz2) ;;
	   *.rar)unrar x $1 && cd $(basename "$1" .rar) ;;
	   *.gz)gunzip $1 && cd $(basename "$1" .gz) ;;
	   *.tar)tar xvf $1 && cd $(basename "$1" .tar) ;;
	   *.tbz2)tar xvjf $1 && cd $(basename "$1" .tbz2) ;;
	   *.tgz)tar xvzf $1 && cd $(basename "$1" .tgz) ;;
	   *.zip)unzip $1 && cd $(basename "$1" .zip) ;;
	   *.Z)uncompress $1 && cd $(basename "$1" .Z) ;;
	   *.7z)7z x $1 && cd $(basename "$1" .7z) ;;
	   *)echo "don't know how to extract '$1'..." ;;
       esac
   else
       echo "'$1' is not a valid file!"
   fi
}