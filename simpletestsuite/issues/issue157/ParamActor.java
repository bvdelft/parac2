public class ParamActor
{
  public final java.lang.Object a;
  static final public se.chalmers.paragon.Lock L = se.chalmers.paragon.Lock.newLock("L", 1);
  public void foo()
  {
    L.open(a);
  }
}