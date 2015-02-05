import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.imageio.ImageIO;

import org.apache.pdfbox.TextToPDF;
import org.apache.pdfbox.exceptions.COSVisitorException;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;

import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.JFileChooser;

public class main {



	public static void main(String args[]){
		selectPdf();
	}


	public static void selectPdf(){


		File docenti = new File("C:\\Users\\nick__000\\Downloads\\docenti.pdf");
		File alunni = new File("C:\\Users\\nick__000\\Downloads\\orario.pdf");

		convertPDFToJPG(docenti.toString(),"Docenti");
		//convertPDFToJPG(alunni.toString(),"Alunni");

		//            }


	}


	public static void convertPDFToJPG(String src, String type){

		try{

			String final_path = "SplittedImages//";
			//load pdf file in the document object
			PDDocument doc=PDDocument.load(new FileInputStream(src));
			//Get all pages from document and store them in a list
			List<PDPage> pages=doc.getDocumentCatalog().getAllPages();
			//create iterator object so it is easy to access each page from the list
			Iterator<org.apache.pdfbox.pdmodel.PDPage> i= pages.iterator();
			int count=1; //count variable used to separate each image file
			//Convert every page of the pdf document to a unique image file
			System.out.println("Please wait...");
			List<File> ListFile = new ArrayList<File>();
			//File Images[] = new File[100]; 
			while(i.hasNext()){
				PDPage page=i.next(); 
				BufferedImage bi=page.convertToImage();
				File file =  new File("ImagesFromPdf//"+"pdfimage"+type+count+".jpg");
				ListFile.add(file);
				count++;
				if(file.exists()){
					System.out.println("File Gia Presente ");
					continue;
				}		
				ImageIO.write(bi, "jpg", file);
				System.out.println("Converting page: "+count+" of "+type);
				
			}



			//splitting the images in two equal parts
			int rows = 2; //You should decide the values for rows and cols variables  
			int cols = 1;  
			int chunks = rows * cols;  
			count = 0;
			String position[]={"top","bottom"};
			int ff=0;
			for(;count<ListFile.size();count++){

				// File file = new File("pdfimage"+type+count+" .jpg");
				FileInputStream fis = new FileInputStream(ListFile.get(count));  
				BufferedImage image = ImageIO.read(fis); //reading the image file 
				int chunkWidth = image.getWidth() / cols; // determines the chunk width and height  
				int chunkHeight = image.getHeight() / rows;  
				
				BufferedImage imgs[] = new BufferedImage[chunks]; //Image array to hold image chunks  
				int index = 0;
			
				for (int x = 0; x < rows; x++) {  
					System.out.println("Splitting image "+count+" of "+type);
					for (int y = 0; y < cols; y++) {  
						//Initialize the image array with image chunks  
						imgs[index] = new BufferedImage(chunkWidth, chunkHeight, image.getType());
						
						 // draws the image chunk  
		                Graphics2D gr = imgs[index].createGraphics();  
		                gr.drawImage(image, 0, 0, chunkWidth, chunkHeight, chunkWidth * y, chunkHeight * x, chunkWidth * y + chunkWidth, chunkHeight * x + chunkHeight, null);  
		                gr.dispose(); 
		                
		                File file2 =new File(final_path+"img" + ff++ + ".jpg");
		                if(file2.exists()){
							System.out.println("File2 Gia Presente ");
							index++;
							continue;
						}		
						ImageIO.write(imgs[index], type+"-"+position[x]+" jpg", file2);  
						index++;
						
					}
				}
			}

			System.out.println("Conversion completed!");
		}
		catch(IOException ie){ie.printStackTrace();}



	}
}