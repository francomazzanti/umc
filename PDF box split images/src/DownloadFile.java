import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;


public class DownloadFile {

  public static void main(String[] args) throws IOException {
		 
	 
		 URL link = new URL("http://www.istitutofermi.it/wp-content/uploads/2010/05/COM_A_160-var.ora_.doc"); //The file that you want to download
		
     //Code to download
		 InputStream in = new BufferedInputStream(link.openStream());
		 ByteArrayOutputStream out = new ByteArrayOutputStream();
		 byte[] buf = new byte[1024];
		 int n = 0;
		 while (-1!=(n=in.read(buf)))
		 {
		    out.write(buf, 0, n);
		 }
		 out.close();
		 in.close();
		 byte[] response = out.toByteArray();
 
		 FileOutputStream fos = new FileOutputStream("File scaricato//"+"File.txt");
		 fos.write(response);
		 fos.close();
     //End download code
		 
		 System.out.println("Finished");

	}

}
 

