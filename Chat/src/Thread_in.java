import java.io.*;
import java.net.*;
public class Thread_in implements Runnable{
Socket sock;
    public Thread_in(Socket sock){
         
        this.sock = sock;
    }
    public void run(){
         
        try{
            BufferedReader bfInput = new BufferedReader(new InputStreamReader(sock.getInputStream()));
            String received = bfInput.readLine();
            System.out.println("Client> "+received);
            bfInput.close();
        }  
        catch(IOException e){
                e.printStackTrace();
            }
         
         
             
             
         
         
    }
}