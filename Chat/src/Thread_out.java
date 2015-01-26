import java.io.*;
import java.net.*;
 
public class Thread_out implements Runnable{
Socket sock;
    public Thread_out(Socket sock){
         
        this.sock = sock;
    }
    public void run(){
         
        try{
            BufferedWriter out = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));
            BufferedReader bfKeyboard = new BufferedReader(new InputStreamReader(System.in));
            String toSend = bfKeyboard.readLine();
            out.write(toSend+"\n");
            out.flush();
            System.out.println("Me> "+toSend);
            out.close();
        }  
        catch(IOException e){
                e.printStackTrace();
            }
         
         
             
             
         
         
    }
     
     
}