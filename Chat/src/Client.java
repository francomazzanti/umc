
	
import java.io.*;
import java.net.*;
public class Client {
 
    public Client()throws IOException{
        
         
        Socket sock = new Socket("146.48.106.198",6789);
        System.out.println("Connessione al server riuscita!");
        /*BufferedReader bfKeyboard = new BufferedReader(new InputStreamReader(System.in));
        BufferedReader bfInput = new BufferedReader(new InputStreamReader(sock.getInputStream()));
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));*/
        String toSend="";
        String received= "";
        /*while(true){
            
            received = bfInput.readLine();
            System.out.println("Client> "+received);
            toSend = bfKeyboard.readLine();
            out.write(toSend+"\n");
            out.flush();
            System.out.println("Server> "+toSend);
        }*/
        while(true){
            
            Thread_out tOut = new Thread_out(sock);
            Thread_in tIn = new Thread_in(sock);
            Thread tI = new Thread(tIn);
            Thread tO = new Thread(tOut);
            tO.start();
            tI.start();
             
        }
    }
    public static void main(String args[]) throws IOException{
    	Client c = new Client();
    	
    }
}


 