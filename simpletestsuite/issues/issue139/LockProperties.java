public class LockProperties
{
  static final public se.chalmers.paragon.Lock L = se.chalmers.paragon.Lock.newLock("L", 2);
  static {
           L.addClause(se.chalmers.paragon.ActorList.newActorList(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Actor.newActorVariable(1)), se.chalmers.paragon.Atom.newAtom(L, se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Actor.newActorVariable(2)), se.chalmers.paragon.Atom.newAtom(L, se.chalmers.paragon.Actor.newActorVariable(2), se.chalmers.paragon.Actor.newActorVariable(1)));
         }
  static final public se.chalmers.paragon.Lock M = se.chalmers.paragon.Lock.newLock("M", 2);
  static {
           M.transitive();
         }
  public static final java.lang.Object alice = new java.lang.Object();
  public static final java.lang.Object bob = new java.lang.Object();
  public static final java.lang.Object charlie = new java.lang.Object();
  public void fooL()
  {
    L.open(alice, bob);
    L.open(bob, charlie);
    int x = 3;
    int y = x;
  }
  public void fooM()
  {
    M.open(alice, bob);
    M.open(bob, charlie);
    int x = 3;
    int y = x;
  }
}