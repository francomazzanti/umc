package com.example.riccardo.progettoapporario;

import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.StrictMode;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Spinner;
import android.widget.Toast;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Calendar;
import java.util.Date;

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
                R.layout.layoutspinner,R.id.textSpinner,
                new String[]{"5AIF","5BIF","4AIF","4ART"}
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
                "8:00;fisica\nY2;Ita\nY6;Mate\nY8;Sistemi\nX11;Informatica\nY4;Religione\nY4;"+
                "9:00;GPO\nX1;Sistemi\nW04;Storia\nY05;Italiano\nZ4;Informatica\nY11;Mate\nY06;"+
                "10:00;Inglese1\nY2;Informatia\nK01;Sistemi\nY04;TPS\nY02;Ed fisica\nPalestra;Italiano\nK1;"+
                "11:00;Inglese\nY2;Informatia\nK01;Sistemi\nY04;TPS\nY02;Ed fisica\nPalestra;Italiano\nK1;"+
                "12:00;Inglese\nY2;Informatia\nK01;Sistemi\nY04;TPS\nY02;Ed fisica\nPalestra;Italiano\nK1;"+
                "13:00;Inglese\nY2;Informatia\nK01;Sistemi\nY04;TPS\nY02;Ed fisica\nPalestra;Italiano\nK1;";

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

    public void popUp(View v){

        // 1. Instantiate an AlertDialog.Builder with its constructor
        AlertDialog.Builder builder;
        builder = new AlertDialog.Builder(this);
        Calendar c = Calendar.getInstance();
        String time = c.getTime().toString();
        Spinner spinnerId = (Spinner)findViewById(R.id.spinner);
// 2. Chain together various setter methods to set the dialog characteristics
        builder.setMessage(SendMesVariazioneOrario((String)spinnerId.getSelectedItem()));
        builder.setTitle("Variazione Orario del "+time);

// 3. Get the AlertDialog from create()
        AlertDialog dialog = builder.create();
        dialog.show();
    }

    public void OnOrarioCompleto(View v){

        Intent intent = new Intent(this, OrarioCompletoActivity.class);
        // EditText editText = (EditText) findViewById(R.id.edit_message);
        // String message = editText.getText().toString();
        // intent.putExtra(EXTRA_MESSAGE, message);
        startActivity(intent);


    }

    public void OnOrarioGiorno(View v){

        Intent intent = new Intent(this,Activity_OrarioGiorno.class);
        // EditText editText = (EditText) findViewById(R.id.edit_message);
        // String message = editText.getText().toString();
        // intent.putExtra(EXTRA_MESSAGE, message);
        startActivity(intent);


    }


    public String SendMesVariazioneOrario(String Classe){
        Socket socket = null;
        DataOutputStream dataOutputStream = null;
        DataInputStream dataInputStream = null;
        String result = "";
        SharedPreferences settings = getSharedPreferences("MyPrefsFile", 0);
        //  boolean silent = settings.getBoolean("silentMode", false);
        String set =settings.getString("DefaultIp","146.48.107.165");

        StrictMode.ThreadPolicy policy = new StrictMode.ThreadPolicy.Builder().permitAll().build();

        StrictMode.setThreadPolicy(policy);

        try {
            socket = new Socket(set, 1800);
            dataOutputStream = new DataOutputStream(socket.getOutputStream());
            dataInputStream = new DataInputStream(socket.getInputStream());
            dataOutputStream.writeUTF(Classe.toString()+"\n");


            byte[] mybytearray = new byte[ 256];



             dataInputStream.read(mybytearray);

            result =  new String(mybytearray);
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        finally{
            if (socket != null){
                try {
                    socket.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

            if (dataOutputStream != null){
                try {
                    dataOutputStream.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

            if (dataInputStream != null){
                try {
                    dataInputStream.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }



        return result;

    }


}
