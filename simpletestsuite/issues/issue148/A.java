public class A
{
  static final public se.chalmers.paragon.Lock ALock = se.chalmers.paragon.Lock.newLock("ALock", 1);
  public void foo(A a)
  {
    ALock.open(a);
  }
}