import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.*;
public class Client {
public static void main ( String args[]){
	Socket sock;
	
	try {
		   sock = new Socket("localhost",7777);
		   System.out.println("Connessione al server riuscita!");
		   BufferedReader input=new BufferedReader(new InputStreamReader(sock.getInputStream()));
		   BufferedReader inputKey=new BufferedReader(new InputStreamReader(System.in));
		   BufferedWriter output=new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));
		   
		   String leggo=input.readLine();
		   System.out.println(leggo);
		   
		   output.close();
		   input.close();
		   sock.close();
	} catch (Exception e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}

	
	
	
}


}
