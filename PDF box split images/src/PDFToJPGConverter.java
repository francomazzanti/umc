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

public class PDFToJPGConverter{
	public static void main(String[] args) throws InvalidFormatException, OpenXML4JException, XmlException, IOException{

		selectPdf();
		//docConverter ();
	}

	//allow images selection for converting
	public static void selectPdf() throws InvalidFormatException, OpenXML4JException, XmlException{

		JFileChooser chooser = new JFileChooser();
		FileNameExtensionFilter filter = new FileNameExtensionFilter("PDF","pdf");
		chooser.setFileFilter(filter);
		chooser.setMultiSelectionEnabled(false);
		int returnVal = chooser.showOpenDialog(null);
		if(returnVal == JFileChooser.APPROVE_OPTION) {
			File file=chooser.getSelectedFile();  
			convertPDFToJPG(file.toString());
		}


	}

	public static void convertPDFToJPG(String src) throws InvalidFormatException, OpenXML4JException, XmlException{

		try{

			//load pdf file in the document object
			PDDocument doc=PDDocument.load(new FileInputStream(src));
			//Get all pages from document and store them in a list
			List<PDPage> pages=doc.getDocumentCatalog().getAllPages();
			//create iterator object so it is easy to access each page from the list
			Iterator<PDPage> i= pages.iterator();
			int count=1; //count variable used to separate each image file
			//Convert every page of the pdf document to a unique image file
			System.out.println("Please wait...");
			while(i.hasNext()){
				PDPage page=i.next();
				BufferedImage bi=page.convertToImage();
				String nfile = "Immagini splittate e non//"+ "pdfimage"+count;
				File nomefile = new File(nfile+".jpg");
				ImageSplitTest(bi,nfile);
				ImageIO.write(bi, "jpg",nomefile );
				count++;



			}
			System.out.println("Conversion complete");
		}catch(IOException ie){ie.printStackTrace();}
	}

	/* CREATA NA CLASSE NUOVA
	public static void docConverter () throws IOException, OpenXML4JException, OpenXML4JException, XmlException{
		//converdionr into doc 
		InputStream fis = new FileInputStream("VARIAZIONI-CALENDARIO-CORSI-DI-RECUPERO-ESTIVI-2014.doc");
		POITextExtractor extractor;
		// if docx

		// if doc
		POIFSFileSystem fileSystem = new POIFSFileSystem(fis);
		extractor = ExtractorFactory.createExtractor(fileSystem);

		String extractedText = extractor.getText();

		FileWriter fw=new FileWriter("FIle convertito.txt");

		fw.write(extractedText);

		fw.close();
		//fine

	}
	*/

	public static void ImageSplitTest(BufferedImage image,String nomeFile) {  


		//reading the image file  

		int rows = 2; //You should decide the values for rows and cols variables  
		int cols = 1;  
		int chunks = rows * cols;  

		int chunkWidth = image.getWidth() / cols; // determines the chunk width and height  
		int chunkHeight = image.getHeight() / rows;  

		BufferedImage imgs[]=new BufferedImage[chunks];
		int count=0;
		//Image array to hold image chunks  
		for (int x = 0; x < rows; x++) {  
			for (int y = 0; y < cols; y++) {  

				//Initialize the image array with image chunks  
				imgs[count] = new BufferedImage(chunkWidth, chunkHeight, image.getType());  

				// draws the image chunk  
				Graphics2D gr = imgs[count].createGraphics();  
				gr.drawImage(image, 0, 0, chunkWidth, chunkHeight, chunkWidth * y, chunkHeight * x, chunkWidth * y + chunkWidth, chunkHeight * x + chunkHeight, null);  
				gr.dispose();  

				try {
					ImageIO.write(imgs[count], "jpg", new File(nomeFile+"img" + count + ".jpg"));
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				count++;

			} 
		}
		System.out.println("Splitting done");  

		//writing mini images into image files  
		/*for (int i = 0; i < imgs.length; i++) {  
			try {
				ImageIO.write(imgs[i], "jpg", new File("img" + i + ".jpg"));
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}  
		}  */
		System.out.println("Mini images created");  

	}  

}
