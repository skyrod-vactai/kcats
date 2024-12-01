# paste these into a shell that you're using to build android libraries
# ensure $TOOLCHAIN/bin has been added to PATH (should already be done here)

export ANDROID_NDK_HOME=/home/v/Android/Sdk/ndk/28.0.12433566
export TOOLCHAIN=$ANDROID_NDK_HOME/toolchains/llvm/prebuilt/linux-x86_64

export TARGET_AARCH64=aarch64-linux-android
export TARGET_X86_64=x86_64-linux-android

export API_LEVEL=35

# aarch64-linux-android
export AR_AARCH64="$TOOLCHAIN/bin/$TARGET_AARCH64"-ar
export CC_AARCH64="$TOOLCHAIN/bin/$TARGET_AARCH64$API_LEVEL"-clang
export CXX_AARCH64="$TOOLCHAIN/bin/$TARGET_AARCH64$API_LEVEL"-clang++

# x86_64-linux-android
export AR_X86_64="$TOOLCHAIN/bin/$TARGET_X86_64"-ar
export CC_X86_64="$TOOLCHAIN/bin/$TARGET_X86_64$API_LEVEL"-clang
export CXX_X86_64="$TOOLCHAIN/bin/$TARGET_X86_64$API_LEVEL"-clang++

export PATH=$PATH:$TOOLCHAIN/bin
