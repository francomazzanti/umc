package com.example.riccardo.progettoapporario;

import android.content.SharedPreferences;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.GridView;
import android.widget.Spinner;
import android.widget.Toast;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Calendar;


public class Activity_OrarioGiorno extends ActionBarActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_activity__orario_giorno);



        Spinner spinner1 = (Spinner)findViewById(R.id.spinner1);
        ArrayAdapter<String> adapter = new ArrayAdapter<String>(
                this,
                R.layout.layoutspinner,R.id.textSpinner,
                new String[]{"Lunedì","Martedì","Mercoledì","Giovedì","Venerdì","Sabato"}
        );
        spinner1.setAdapter(adapter);



        // Spinner spinnerId = (Spinner)findViewById(R.id.spinner);
        // int text = spinnerId.getSelectedItemPosition();

        spinner1.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            public void onItemSelected(AdapterView<?> adapter, View view,int pos, long id) {
                String selected = (String)adapter.getItemAtPosition(pos);
                Toast.makeText(
                        getApplicationContext(),
                        "hai selezionato " + selected,
                        Toast.LENGTH_LONG
                ).show();



                int dayOfWeek = pos;

                ArrayAdapter<String> adaptergrid=new ArrayAdapter<String>(adapter.getContext(),android.R.layout.simple_list_item_1,getOrarioGiorno(dayOfWeek));

                // sostituiamo ListView con GridView
                GridView gridView = (GridView) findViewById(R.id.gridView2);
                gridView.setAdapter(adaptergrid);


            }
            public void onNothingSelected(AdapterView<?> arg0) {}
        });


        //setto lo spinner per il giorno corrente
        Calendar c = Calendar.getInstance();
        //c.setFirstDayOfWeek(Calendar.SUNDAY);

        int dayOfWeek = c.get(Calendar.DAY_OF_WEEK)-2;
        if(dayOfWeek>0) {
            spinner1.setSelection(dayOfWeek);
        }else{
            spinner1.setSelection(0);
        }

        //inserisco i dati nel grid per il giorno corrente


        ArrayAdapter<String> adaptergrid=new ArrayAdapter<String>(this,android.R.layout.simple_list_item_1,getOrarioGiorno(dayOfWeek));

        // sostituiamo ListView con GridView
        GridView gridView = (GridView) findViewById(R.id.gridView2);
        gridView.setAdapter(adaptergrid);
    }

    public String[] getOrarioGiorno(int day){


        String readString ="";
        try {
            FileInputStream fos = openFileInput("Orario.txt");
            byte[] input = new byte[fos.available()];
            while (fos.read(input) != -1) {
                readString += new String(input);
            }
            fos.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        int contatore = 0;

        readString =  readString.replaceAll("\n","");
        readString =  readString.replaceAll("#","\n");
        String [] element =  readString.split(";");
            String [] result=new String[12];
        for(int i=1;i<7;i++){
            String Orario = element[i*7];
            String MateriaAula = element[day+1+(7*i)];
            result[contatore]=Orario;
            contatore++;
            result[contatore]=MateriaAula;
            contatore++;
        }


        return result;

    }
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
       // getMenuInflater().inflate(R.menu.menu_activity__orario_giorno, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
}
