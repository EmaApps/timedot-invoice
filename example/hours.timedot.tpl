<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    Invoice
    <invoice:date /> - Sridhar Ratnakumar
  </title>
  <link href="https://unpkg.com/tailwindcss@2/dist/tailwind.min.css" rel="stylesheet" type="text/css">
</head>

<!-- TODO: Allow overriding these from CLI (or foo.timedot.tpl.yaml)? -->
<!-- DoNotFormat -->
<bind tag="theme">blue</bind>
<bind tag="iconSize">w-4 h-4 flex-shrink-0</bind>
<!-- DoNotFormat -->

<body class="overflow-y-scroll">
  <p class="text-red-500 text-3xl">Hello</p>
  <pre class="overflow-y-scroll"><invoice:hours /></pre>
  <invoice:hours>
    <hour>
      <em>
        <hour:day />
      </em>
      -
      <hour:clients>
        <client>
          <li>
            <b>
              <client:name />
            </b>
            -
            <tt>
              <client:hours />
            </tt>
          </li>
        </client>

      </hour:clients>
    </hour>
  </invoice:hours>
</body>

</html>