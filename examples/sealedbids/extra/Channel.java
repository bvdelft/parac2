import java.util.Scanner;

public class Channel
{
  private Bidder b;
  public Channel (se.chalmers.paragon.runtime.Policy pol, Bidder b)
  {
    this.b = b;
  }
  public int get() throws NoBidException
  {
    Scanner in = new Scanner(System.in);
    try {
      System.out.print("Bid for bidder " + b.id + ": ");
      return in.nextInt();
    } catch (Exception e) {
      throw new NoBidException();
    }
  }
  public void put(int winBid) {
    System.out.println("Bidder " + b.id + " receives the winning bid was " + winBid);
  }
  
  public void put(java.lang.String data) {
    System.out.println("Bidder " + b.id + " receives:");
    System.out.println(data);
  }
}
