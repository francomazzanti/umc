import java.io.*;
import java.net.*;
import java.util.HashMap;
import java.util.Map;

import org.apache.poi.POITextExtractor;
import org.apache.poi.extractor.ExtractorFactory;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.xmlbeans.XmlException;

public class Thread1800 implements Runnable {
	public Thread1800() {


	}

	public void run(){
		ServerSocket sock;
		try {
			while(true) {	
				sock=new ServerSocket(1800);
				Socket sock1= sock.accept(); 
				BufferedReader input=new BufferedReader(new InputStreamReader(sock1.getInputStream()));
				//BufferedReader inputKey=new BufferedReader(new InputStreamReader(System.in));
				//BufferedWriter output=new BufferedWriter(new OutputStreamWriter(sock1.getOutputStream())) ;
				OutputStream os = sock1.getOutputStream();
				System.out.println("Connessione al server riuscita!");

				//Thread.sleep(2000);
				
				String clientSentence = input.readLine();
				System.out.println(clientSentence);
				String result = docConverter();
				
				Map<String,String> mappa = Split(result);
				
				
				if(mappa.containsKey(clientSentence.trim())) {
					
					String varorario = mappa.get(clientSentence.trim());
					System.out.println(mappa.get(clientSentence.trim()));
					
					os.write((varorario+"\n").getBytes());
					//

				}else{
					
					
					os.write("Varizione non presente\n".getBytes());
				}
				
				
				
				
				//output.write("ciao, sto mandando, questo è il messaggio /n");
				System.out.println("Ho mandato");


				os.close();
				input.close();
				sock.close();
			}

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}


	}


	public static String docConverter (){
		//converdionr into doc 
		InputStream fis;
		String extractedText = "Problemi Di Conversione"; 
		try {
			URL link = new URL("http://www.istitutofermi.it/wp-content/uploads/2010/05/COM_A_160-var.ora_.doc"); //The file that you want to download

			//Code to download
			InputStream in = new BufferedInputStream(link.openStream());


			// if docx

			// if doc
			POIFSFileSystem fileSystem = new POIFSFileSystem(in);
			POITextExtractor extractor = ExtractorFactory.createExtractor(fileSystem);

			 extractedText = extractor.getText();



			System.out.println("Convertito!");

			System.out.println("splitto il file convertito");
			
			

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidFormatException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OpenXML4JException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XmlException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return extractedText;
	}

	//fine



	public static Map<String,String> Split(String extractedText)  {

		String[] broken_text;
		//	word=extractedText.split("/t");


		BufferedReader reader = new BufferedReader(new StringReader(extractedText));

		Map<String,String> mapHash=new HashMap<String,String>();
		try {
			while((extractedText = reader.readLine()) != null) {
				broken_text = extractedText.split("\t");
				if(broken_text.length>0){
					String first_key = broken_text[0];

					if (first_key.equals("Classe")) {
						extractedText = reader.readLine();
						while(extractedText.length()>3) {
							String[] broken_text1;
							broken_text1 = extractedText.split("\t");
							String key;
							key=broken_text1[0];
							String map;
							map=broken_text1[1]; 
							String	key1=key.replace("^","");
							String key2=key1.replaceAll("\\s+","");
							mapHash.put(key2,map);
							extractedText = reader.readLine();
						}
						break;
					}
				}
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println(mapHash);
		
		return mapHash;

		

	}


}
