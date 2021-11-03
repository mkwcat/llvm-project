// RUN: %clang_cc1 %s -triple=powerpc-gekko-ibm-kuribo-eabi -emit-llvm-only

struct VtableAt0 {
  virtual void vfunc() {}
  int _;
};

struct VtableAt4 {
  int _;
  virtual void vfunc() {}
};