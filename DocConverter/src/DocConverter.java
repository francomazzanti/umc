import org.apache.poi.POITextExtractor;
import org.apache.poi.extractor.ExtractorFactory;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.xwpf.extractor.XWPFWordExtractor;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.xmlbeans.XmlException;

import java.io.*;
import java.util.HashMap;
import java.util.Scanner;



public class DocConverter {

	public static void main(String[] args) {
		try{
			//convert();
			findTimetables();
		}
		catch(Exception e){
			e.printStackTrace();
		}
	}


	public static void convert()throws XmlException, IOException,OpenXML4JException{
		InputStream fis = new FileInputStream("Doc\\Variazione orario.doc");
		POITextExtractor extractor;
		String fileName = fis.toString();
		if (fileName.toLowerCase().endsWith(".docx")) {
			System.out.println("Converting .DOCX file to .txt");
			XWPFDocument doc = new XWPFDocument(fis);
			extractor = new XWPFWordExtractor(doc);

		} else {
			System.out.println("Converting .DOC file to .txt");
			POIFSFileSystem fileSystem = new POIFSFileSystem(fis);
			extractor = ExtractorFactory.createExtractor(fileSystem);
		}
		String extractedText = extractor.getText();
		PrintWriter out = new PrintWriter("Ora//VariazioneOra.txt");
		out.println(extractedText);
		System.out.println("File Converted!");
		out.close();

	}

	public static void findTimetables() throws IOException{
		//BufferedReader file = new BufferedReader (new FileReader("Ora\\VariazioneOra.txt"));
		BufferedReader input = new BufferedReader(new InputStreamReader(System.in));
		HashMap<String, String> timetables = new HashMap<String, String>();

		String searchFor = "Classe	Entrata/Uscita della classe	NOTE";
		String line = "";
		/*while(file.readLine()!=" "){
			System.out.println("looking for "+searchFor);
			if(file.readLine().startsWith(searchFor)){
				file.readLine();
				while((line=file.readLine())!=" "){

					timetables.put(line.split("\t").toString(), line.substring(line.split("\t").length, 50));

				}
				break;
			}
		}*/
		File file = new File("Ora//VariazioneOra.txt");
		Scanner txt = new Scanner(file);
		/*List<String> list=new ArrayList<>();
		while(txt.hasNextLine())
			list.add(txt.nextLine()); */
		while(txt.hasNextLine()){
			line = txt.nextLine();
			if(searchFor.trim().equalsIgnoreCase(line.trim())){

				while(!line.equals("\t\t")){
					line =txt.nextLine();
					if(!line.equals("\t\t")){
						String[] riga =line.split("\t");
						timetables.put(riga[0].trim(), riga[1]);
					}else{
						break;
					}

				}

				break;
			}


		}
		System.out.println(timetables);
		System.out.println("Please insert the class you wanna have the timetable of:");
		searchFor = input.readLine();
		if(timetables.containsKey(searchFor))
			System.out.println(timetables.get(searchFor)+"\n");



	}

	/*public static void DownloadFile() throws IOException{
		 String fileName = "file.txt"; //The file that will be saved on your computer
		 URL link = new URL("http://www.istitutofermi.it/wp-content/uploads/2014/06/Calendario_Scolastico_2014_15.pdf"); //The file that you want to download

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

		 FileOutputStream fos = new FileOutputStream(fileName);
		 fos.write(response);
		 fos.close();
     //End download code

		 System.out.println("Finished");

	}*/

}
