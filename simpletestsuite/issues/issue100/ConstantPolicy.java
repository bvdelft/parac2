public class ConstantPolicy
{
  public void m(int i)
  {
  }
  public void foo()
  {
    int x = 4;
    m(x);
  }
  public void bar()
  {
    m(4);
  }
}