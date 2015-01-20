import org.xml.sax.*; 
import org.xml.sax.helpers.*;

public class XmlValidate
{ 
    public static void main(final String[] args) 
    { 
        try { 
            if (args.length != 4) { 
                System.out.println("Incorrect arguments"); 
                System.out.println("parameters: xml-file xsd-file xml-file-directory"); 
                
            }
            else {

                XMLReader r = XMLReaderFactory.createXMLReader(); 
                System.out.println("-*- mode: grep; default-directory: \"" + args[2] + "\" -*-\n");

                r.setErrorHandler(new ErrorHandler() { 

                        public void error(SAXParseException ex) throws SAXException { 
                            System.out.println("./" + args[3] + ":" + ex.getLineNumber() + ":-" + ex.getMessage()); 
                        } 

                        public void fatalError(SAXParseException arg0) throws SAXException { 
                            throw(arg0); 
                        } 

                        public void warning(SAXParseException arg0) throws SAXException { 
                            System.out.println("warning: " + arg0.getMessage()); 
                        } 
                    }); 

                r.setFeature("http://xml.org/sax/features/validation", true); 
                r.setFeature("http://xml.org/sax/features/namespaces", true); 
                r.setFeature("http://apache.org/xml/features/validation/schema", true); 
                r.setProperty("http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation", args[1]); 
                
                r.parse(new InputSource(args[0])); 
                System.out.println(args[0] + " was succesfully validated against schema " + args[1]); 
            }

        } 
        catch (Exception e) { 
            e.printStackTrace(); 
        } 
    } 
} 