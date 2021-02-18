// RUN: %clang_cc1 %s -triple=powerpc-unknownn-kuribo -emit-llvm-only -fdump-vtable-layouts > %t
// RUN: FileCheck --check-prefix=CHECK-1 %s < %t
// RUN: FileCheck --check-prefix=CHECK-2 %s < %t

// For now, just verify this doesn't crash.
namespace test0 {
  struct Obj {};

  struct Base {           virtual const Obj *foo() = 0; };
  struct Derived : Base { virtual       Obj *foo() { return new Obj(); } };

  void test(Derived *D) { D->foo(); }
}

namespace Test1 {
// CHECK-1:      Vtable for 'Test1::A' (3 entries).
// CHECK-1-NEXT:   0 | Test1::A RTTI
// CHECK-1-NEXT:   1 | offset_to_top (0)
// CHECK-1-NEXT:       -- (Test1::A, 0) vtable address --
// CHECK-1-NEXT:   2 | void Test1::A::f()
//
// CHECK-1:      VTable indices for 'Test1::A' (1 entries).
// CHECK-1-NEXT:   0 | void Test1::A::f()
struct A {
  virtual void f();
};
void A::f() { }

}


namespace Test2 {

// This is a smoke test of the vtable dumper.
// CHECK-2:      Vtable for 'Test2::A' (8 entries).
// CHECK-2-NEXT:   0 | Test2::A RTTI
// CHECK-2-NEXT:   1 | offset_to_top (0)
// CHECK-2-NEXT:       -- (Test2::A, 0) vtable address --
// CHECK-2-NEXT:   2 | void Test2::A::f()
// CHECK-2-NEXT:   3 | void Test2::A::f() const
// CHECK-2-NEXT:   4 | Test2::A *Test2::A::g(int)
// CHECK-2-NEXT:   5 | Test2::A::~A() [complete]
// CHECK-2-NEXT:   6 | void Test2::A::h()
// CHECK-2-NEXT:   7 | Test2::A &Test2::A::operator=(const Test2::A &)
//
// CHECK-2:      VTable indices for 'Test2::A' (6 entries).
// CHECK-2-NEXT:   0 | void Test2::A::f()
// CHECK-2-NEXT:   1 | void Test2::A::f() const
// CHECK-2-NEXT:   2 | Test2::A *Test2::A::g(int)
// CHECK-2-NEXT:   3 | Test2::A::~A() [complete]
// CHECK-2-NEXT:   4 | void Test2::A::h()
// CHECK-2-NEXT:   5 | Test2::A &Test2::A::operator=(const Test2::A &)
struct A {
  virtual void f();
  virtual void f() const;
  
  virtual A* g(int a);
  virtual ~A();
  virtual void h();
  virtual A& operator=(const A&);
};
void A::f() { }

}