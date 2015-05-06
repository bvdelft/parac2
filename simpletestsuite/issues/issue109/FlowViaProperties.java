public class FlowViaProperties
{
  public static final se.chalmers.paragon.Policy bottom = se.chalmers.paragon.Policy.newPolicy("bottom", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0)));
  static final public se.chalmers.paragon.Lock A = se.chalmers.paragon.Lock.newLock("A", 0);
  static final public se.chalmers.paragon.Lock B = se.chalmers.paragon.Lock.newLock("B", 0);
  static {
           B.addClause(se.chalmers.paragon.ActorList.newActorList(), se.chalmers.paragon.Atom.newAtom(A));
         }
  public static final se.chalmers.paragon.Policy secret = se.chalmers.paragon.Policy.newPolicy("secret", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(B)));
  int pub;
  int sec;
  public void something()
  {
    A.open();
    if (B)
    {
      A.close();
      pub = sec;
    }
  }
}