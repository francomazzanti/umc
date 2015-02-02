package com.example.riccardo.progettoapporario;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Spinner;
import android.widget.Toast;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

//savedInstanceState

public class PannelloPrincipale extends ActionBarActivity {
    public static final String PREFS_NAME = "MyPrefsFile";


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_pannello_principale);

        Spinner spinner = (Spinner)findViewById(R.id.spinner);
        ArrayAdapter<String> adapter = new ArrayAdapter<String>(
                this,
                android.R.layout.simple_spinner_item,
                new String[]{"5AIF","5BIF","paperino","topolino"}
        );
        spinner.setAdapter(adapter);



       // Spinner spinnerId = (Spinner)findViewById(R.id.spinner);
       // int text = spinnerId.getSelectedItemPosition();

        spinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            public void onItemSelected(AdapterView<?> adapter, View view,int pos, long id) {
                String selected = (String)adapter.getItemAtPosition(pos);
                Toast.makeText(
                        getApplicationContext(),
                        "hai selezionato "+selected,
                        Toast.LENGTH_LONG
                ).show();

            }
            public void onNothingSelected(AdapterView<?> arg0) {}
        });

        // leggere o creare
        SharedPreferences settings = getSharedPreferences(PREFS_NAME, 0);
      //  boolean silent = settings.getBoolean("silentMode", false);
         int set =settings.getInt("SpinPosition", 0);
        spinner.setSelection(set);

    }
    @Override
    protected void onStop(){
        super.onStop();

        // We need an Editor object to make preference changes.
        // All objects are from android.context.Context
        SharedPreferences settings = getSharedPreferences(PREFS_NAME, 0);
        SharedPreferences.Editor editor = settings.edit();
        Spinner spinnerId = (Spinner)findViewById(R.id.spinner);
        int position = spinnerId.getSelectedItemPosition();
        editor.putInt("SpinPosition",position);
        // Commit the edits!
        editor.commit();
    }




    public void OnClose(MenuItem m){
    finish();

    }


    public void OnSetting(MenuItem m){

        Intent intent = new Intent(this, SettingsActivity.class);
       // EditText editText = (EditText) findViewById(R.id.edit_message);
       // String message = editText.getText().toString();
       // intent.putExtra(EXTRA_MESSAGE, message);
        startActivity(intent);


    }
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_pannello_principale, menu);
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

    public void OnAggiornaOrario(View v){

        String string = ";Lunedi;Martedi;Mercoledi;Giovedi;Venerdi;Sabato;" +
                "8:00/9:00;fisica;Ita;Mate;Infor;Stocazzo;Religione;";
        FileOutputStream fos = null;
        try {
            fos = openFileOutput("Orario.txt", Context.MODE_PRIVATE);
            fos.write(string.getBytes());
            fos.close();

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }


    }

    public void OnOrarioCompleto(View v){

        Intent intent = new Intent(this, OrarioCompletoActivity.class);
        // EditText editText = (EditText) findViewById(R.id.edit_message);
        // String message = editText.getText().toString();
        // intent.putExtra(EXTRA_MESSAGE, message);
        startActivity(intent);


    }


}
