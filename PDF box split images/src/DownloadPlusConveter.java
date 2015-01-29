
  

import java.io.IOException;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.InputStream;

import javax.imageio.ImageIO;  

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

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

public class DownloadPlusConveter {
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
		//fine

	}


}
