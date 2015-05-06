public class TypeMethod
{
  static final public se.chalmers.paragon.Lock Flow = se.chalmers.paragon.Lock.newLock("Flow", 2);
  public static se.chalmers.paragon.Policy pol(java.lang.Object a)
  {
    return se.chalmers.paragon.Policy.newPolicy("", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(Flow, a, se.chalmers.paragon.Actor.newActorVariable(0))));
  }
  final java.lang.Object alice = new java.lang.Object();
  public void test()
  {
    int a = 4;
  }
}