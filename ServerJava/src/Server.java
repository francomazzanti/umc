import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.*;
public class Server {
public static void main ( String args[]){
	ServerSocket sock;
	 
	try {
		while(true) {	
		sock=new ServerSocket(7777);
			Socket sock1= sock.accept(); 
		   System.out.println("Connessione al server riuscita!");
		   BufferedReader input=new BufferedReader(new InputStreamReader(sock1.getInputStream()));
		   BufferedReader inputKey=new BufferedReader(new InputStreamReader(System.in));
		   BufferedWriter output=new BufferedWriter(new OutputStreamWriter(sock1.getOutputStream()));
		   
		    output.write("ciao, sto mandando, questo è il messaggio /n");
		   System.out.println("Ho mandato");
		   
 		   input.close();
		   output.close();
		}
		   
	} catch (Exception e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
		}

	
	
	
}


}
