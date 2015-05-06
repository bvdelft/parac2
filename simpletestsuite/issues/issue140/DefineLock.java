public class DefineLock
{
  static final public se.chalmers.paragon.Lock MyLock = se.chalmers.paragon.Lock.newLock("MyLock", 2);
  static {
           MyLock.addClause(se.chalmers.paragon.ActorList.newActorList(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Actor.newActorVariable(1)), se.chalmers.paragon.Atom.newAtom(MyLock, se.chalmers.paragon.Actor.newActorVariable(1), se.chalmers.paragon.Actor.newActorVariable(0)));
         }
}