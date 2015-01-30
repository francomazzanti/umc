


import java.io.IOException;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;

import javax.imageio.ImageIO;  

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.imageio.ImageIO;

import org.apache.pdfbox.TextToPDF;
import org.apache.pdfbox.exceptions.COSVisitorException;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.poi.POITextExtractor;
import org.apache.poi.extractor.ExtractorFactory;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.xwpf.extractor.XWPFWordExtractor;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.xmlbeans.XmlException;

import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.JFileChooser;

import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.openxml4j.exceptions.OpenXML4JException;
import org.apache.xmlbeans.XmlException;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

public class DownloadPlusConveterPlusDetecCHar {
	public static void main(String[] args) throws InvalidFormatException, OpenXML4JException, XmlException, IOException{

		DowloadFile();
		try {
			Thread.sleep(4000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		docConverter ();

	}



	public static void DowloadFile() throws IOException {


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

		System.out.println("Scaricato!");

	}







	public static void docConverter () throws IOException, OpenXML4JException, OpenXML4JException, XmlException{
		//converdionr into doc 
		InputStream fis = new FileInputStream("File scaricato//"+"file.txt");
		POITextExtractor extractor;
		// if docx

		// if doc
		POIFSFileSystem fileSystem = new POIFSFileSystem(fis);
		extractor = ExtractorFactory.createExtractor(fileSystem);

		String extractedText = extractor.getText();


		FileWriter fw=new FileWriter("File doc convertito//"+"file.txt");

		fw.write(extractedText);
		System.out.println("Convertito!");
		fw.close();

		System.out.println("splitto il file convertito");
		Split(extractedText);


	}

	//fine



	public static void Split(String extractedText) throws IOException {

		String[] broken_text;
		//	word=extractedText.split("/t");


		BufferedReader reader = new BufferedReader(new StringReader(extractedText));

		Map<String,String> mapHash=new HashMap<String,String>();
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
		System.out.println(mapHash);
 		BufferedReader reader1 = new BufferedReader(new InputStreamReader(System.in));
		String inputKey=reader1.readLine();
		
		if(mapHash.containsKey(inputKey)) {
			System.out.println(mapHash.get(inputKey));
			
		}

	}
}
