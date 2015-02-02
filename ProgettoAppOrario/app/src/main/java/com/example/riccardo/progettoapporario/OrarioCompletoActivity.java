package com.example.riccardo.progettoapporario;

import android.content.Context;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ArrayAdapter;
import android.widget.GridView;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;


public class OrarioCompletoActivity extends ActionBarActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_orario_completo);
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

        String [] gridelement =  readString.split(";");
        ArrayAdapter<String> adapter=new ArrayAdapter<String>(this,android.R.layout.simple_list_item_1,gridelement);

        // sostituiamo ListView con GridView
        GridView gridView = (GridView) findViewById(R.id.gridView);
        gridView.setAdapter(adapter);


    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
       //( getMenuInflater().inflate(R.menu.menu_orario_completo, menu);
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
