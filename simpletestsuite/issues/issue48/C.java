public class C
{
  static final public se.chalmers.paragon.Lock SomeUnaryLock = se.chalmers.paragon.Lock.newLock("SomeUnaryLock", 1);
  static final public se.chalmers.paragon.Lock SomeNonArayLock = se.chalmers.paragon.Lock.newLock("SomeNonArayLock", 0);
  public static final java.lang.Object alice = new java.lang.Object();
  public void foo()
  {
    if (SomeNonArayLock)
    {
    }
    if (SomeUnaryLock.isOpen(alice))
    {
    }
  }
}