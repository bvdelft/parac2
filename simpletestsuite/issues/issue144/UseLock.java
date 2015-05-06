public class UseLock
{
  public static final java.lang.Object alice = new java.lang.Object();
  public static final java.lang.Object bob = new java.lang.Object();
  public static final se.chalmers.paragon.Policy polA = se.chalmers.paragon.Policy.newPolicy("polA", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(DefineLock.MyLock, se.chalmers.paragon.Actor.newActorVariable(0), alice)));
  public static final se.chalmers.paragon.Policy polB = se.chalmers.paragon.Policy.newPolicy("polB", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(DefineLock.MyLock, alice, se.chalmers.paragon.Actor.newActorVariable(0))));
  public void foo()
  {
    DefineLock.MyLock.open(alice, bob);
    int b = 4;
    int x1 = b;
    int a = 4;
    int x2 = a;
    int ab = a;
  }
  static final public se.chalmers.paragon.Lock L = se.chalmers.paragon.Lock.newLock("L", 2);
  static {
           L.symmetric();
         }
  public static final se.chalmers.paragon.Policy myPolA = se.chalmers.paragon.Policy.newPolicy("myPolA", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(L, se.chalmers.paragon.Actor.newActorVariable(0), alice)));
  public static final se.chalmers.paragon.Policy myPolB = se.chalmers.paragon.Policy.newPolicy("myPolB", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(L, alice, se.chalmers.paragon.Actor.newActorVariable(0))));
  public void bar()
  {
    L.open(alice, bob);
    int b = 4;
    int x1 = b;
    int a = 4;
    int x2 = a;
    int ab = a;
  }
}