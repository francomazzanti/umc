package com.example.riccardo.progettoapporario;

import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Handler;
import android.os.Message;
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
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Calendar;
import java.util.Date;

//savedInstanceState

public class PannelloPrincipale extends ActionBarActivity {
    public static final String PREFS_NAME = "MyPrefsFile";



    Handler mHandler;

    Handler mHandlerOrarioSettimanale;
    public void messageDisplay(String servermessage)
    {
        AlertDialog.Builder builder;
        builder = new AlertDialog.Builder(this);
        builder.setMessage(servermessage);
        Calendar c = Calendar.getInstance();
        String time = c.getTime().toString();
        builder.setTitle("Variazione Orario del "+time);

// 3. Get the AlertDialog from create()
        AlertDialog dialog = builder.create();
        dialog.show();
    }


    public void messageDisplayOrario(String servermessage)
    {
        Toast.makeText(
                getApplicationContext(),
                servermessage,
                Toast.LENGTH_LONG
        ).show();
    }

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

        mHandler = new Handler() {
            public void handleMessage(Message msg){
                messageDisplay(msg.obj.toString());
            }
        };


        mHandlerOrarioSettimanale = new Handler() {
            public void handleMessage(Message msg){
                messageDisplayOrario(msg.obj.toString());
            }
        };

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

       /* String string = ";Lunedi;Martedi;Mercoledi;Giovedi;Venerdi;Sabato;" +
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
*/


        new Thread(new ClientThreadAggOrario()).start();
    }

    public void popUp(View v){

        // 1. Instantiate an AlertDialog.Builder with its constructor

        new Thread(new ClientThreadVariazioneOrario()).start();


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




    class ClientThreadVariazioneOrario implements Runnable {

        @Override
        public void run() {

            try {
                SharedPreferences settings = getSharedPreferences("MyPrefsFile", 0);
                //  boolean silent = settings.getBoolean("silentMode", false);
                String set =settings.getString("DefaultIp","146.48.107.165");
                InetAddress serverAddr = InetAddress.getByName(set);

                Socket  socketVarazioneOrario = new Socket(serverAddr, 1800);



                Spinner spinnerId = (Spinner)findViewById(R.id.spinner);
// 2. Chain together various setter methods to set the dialog characteristics


                DataOutputStream dataOutputStream = null;
                DataInputStream dataInputStream = null;
                String result = "";


                try {

                    dataOutputStream = new DataOutputStream(socketVarazioneOrario.getOutputStream());
                    dataInputStream = new DataInputStream(socketVarazioneOrario.getInputStream());
                    dataOutputStream.writeUTF((String)spinnerId.getSelectedItem().toString()+"\n");


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
                    if (socketVarazioneOrario != null){
                        try {
                            socketVarazioneOrario.close();
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


                Message serverMessage= Message.obtain();
                serverMessage.obj=result;
                mHandler.sendMessage(serverMessage);


            } catch (UnknownHostException e1) {
                e1.printStackTrace();
            } catch (IOException e1) {
                e1.printStackTrace();
            }

        }

    }

    class ClientThreadAggOrario implements Runnable {

        @Override
        public void run() {

            try {
                SharedPreferences settings = getSharedPreferences("MyPrefsFile", 0);
                //  boolean silent = settings.getBoolean("silentMode", false);
                String set =settings.getString("DefaultIp","146.48.107.165");
                InetAddress serverAddr = InetAddress.getByName(set);

                Socket    socketOrario = new Socket(serverAddr, 1801);



                Spinner spinnerId = (Spinner)findViewById(R.id.spinner);
// 2. Chain together various setter methods to set the dialog characteristics


                DataOutputStream dataOutputStream = null;
                DataInputStream dataInputStream = null;
                String result = "";


                try {

                    dataOutputStream = new DataOutputStream(socketOrario.getOutputStream());
                    dataInputStream = new DataInputStream(socketOrario.getInputStream());
                    dataOutputStream.writeUTF((String)spinnerId.getSelectedItem().toString()+"\n");


                    byte[] mybytearray = new byte[2024];

                    int letti =0;
                    while((letti = dataInputStream.read(mybytearray))>0){

                    }


                    result =  new String(mybytearray);
                } catch (UnknownHostException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                finally{
                    if (socketOrario != null){
                        try {
                            socketOrario.close();
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
                Message serverMessage= Message.obtain();
                if(result.length()>40){
                    serverMessage.obj="File Scaricato Correttamente";
                    FileOutputStream fos = null;
                    try {
                        fos = openFileOutput("Orario.txt", Context.MODE_PRIVATE);
                        fos.write(result.getBytes());
                        fos.close();

                    } catch (FileNotFoundException e) {
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }else{

                    serverMessage.obj=result;

                }
                mHandlerOrarioSettimanale.sendMessage(serverMessage);


            } catch (UnknownHostException e1) {
                e1.printStackTrace();
            } catch (IOException e1) {
                e1.printStackTrace();
            }

        }

    }


}
