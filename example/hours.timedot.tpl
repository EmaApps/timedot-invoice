<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    Invoice
    -
    <invoice:metadata>
      <value var="name" />
    </invoice:metadata>
  </title>
  <link href="https://unpkg.com/tailwindcss@2/dist/tailwind.min.css" rel="stylesheet" type="text/css">
</head>

<!-- DoNotFormat -->
<bind tag="theme"><invoice:metadata><value var="theme" /></invoice:metadata></bind>
<!-- DoNotFormat -->

<body class="overflow-y-scroll">
  <header class="text-4xl text-center border-b-2 py-2 ">
    Invoice
    -
    <invoice:metadata>
      <value var="name" />
    </invoice:metadata>
  </header>

  <p class="text-${theme}-500 text-3xl">Hello</p>
  <div class="container mx-auto max-w-screen-lg">
    <invoice:errors>
      <error>
        <li class="text-red-500">
          <error:err />
        </li>
      </error>
    </invoice:errors>
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
  </div>
</body>

</html>