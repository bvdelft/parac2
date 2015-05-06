public class Panics
{
  public final java.lang.Object id = new java.lang.Object();
  public final se.chalmers.paragon.Policy p = se.chalmers.paragon.Policy.newPolicy("p", se.chalmers.paragon.Policy.newPClause(id));
  void foo()
  {
    Panics c;
    if (c != null)
    {
      int x1;
      int x2;
      se.chalmers.paragon.Policy q = c.p;
      int x3;
      final java.lang.Object a = c.id;
      int x4 = 4;
      int y = x4;
      java.lang.String[] x5;
    }
  }
}