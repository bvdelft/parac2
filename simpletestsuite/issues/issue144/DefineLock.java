public class DefineLock
{
  static final public se.chalmers.paragon.Lock MyLock = se.chalmers.paragon.Lock.newLock("MyLock", 2);
  static {
           MyLock.symmetric();
         }
}