<html>
    <head>
        <title>Example</title>
    </head>
    <body>
        Temp filename: <?php echo $_FILES['userfile']['tmp_name']; ?><br>
        Uploaded filename: <?php echo $_FILES['userfile']['name']; ?><br>
        Uploaded size: <?php echo $_FILES['userfile']['size']; ?><br>
        Uploaded MIME type: <?php echo $_FILES['userfile']['type']; ?><br>
    </body>
</html>
