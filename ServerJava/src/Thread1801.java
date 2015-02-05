import java.io.*;
import java.net.*;

public class Thread1801 implements Runnable {

	public Thread1801() {
		

	}

	public void run(){
		ServerSocket sock;
		try {
			while(true) {	
				sock=new ServerSocket(1801);
				Socket sock1= sock.accept(); 
				BufferedReader input=new BufferedReader(new InputStreamReader(sock1.getInputStream()));
				//BufferedReader inputKey=new BufferedReader(new InputStreamReader(System.in));
				//BufferedWriter output=new BufferedWriter(new OutputStreamWriter(sock1.getOutputStream())) ;
				OutputStream os = sock1.getOutputStream();
				System.out.println("Connessione al server riuscita!");

				//Thread.sleep(2000);
				
				String clientSentenceClasse = input.readLine();
				File f = new File("resource/"+clientSentenceClasse+".txt");
				if(f.exists() && !f.isDirectory()) { 
					/* do something */ 
					byte[] mybytearray = new byte[(int) f.length()];
				      BufferedInputStream bis = new BufferedInputStream(new FileInputStream(f));
				      bis.read(mybytearray, 0, mybytearray.length);
				      os.write(mybytearray, 0, mybytearray.length);
				      os.flush();
				     // sock.close();
				
				      System.out.println("Ho mandato");
				}
				
				//output.write("ciao, sto mandando, questo è il messaggio /n");
				


				os.close();
				input.close();
				sock.close();
			}

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}


	}

}
