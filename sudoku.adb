-----
-- @Author    Matt Emborsky <mlemborsky01@indianatech.net>
-- @Package   Sudoku Puzzle Game
-- @Version   1.8
-- @Date      May 28, 2007
-----

-- Variable prefixes
-- n = Numberical (Integer or Float)
-- str = String
-- a = Array
-- f = Boolean
-- h = handle
-- obj = Object (instance of a Class)
-- r = Range
-- t = SubType or Type
-- p = Package

-- Use camelCaps for naming of everything.

With Ada.Text_IO;
With Ada.Integer_Text_IO;
With Ada.Command_Line;
With Ada.Numerics.Discrete_Random;
With Stacks;



Procedure sudoku is

-- |
-- | Definition of all items related to each individual element
-- | of the sudoku puzzle.
-- |
  SubType rValidValues is Positive Range 1..9; -- The range of valid values used in the Sudoku game.
  SubType rDifficulty is Positive Range 1..3; -- The range of valid possible difficulties.
  SubType rBoxIndex is Positive Range 1..81; -- The range of valid index box values.

  Type tBoxStats is Record
    value:      Integer Range 0..9 := 0;    -- Holds the actual boxes value.
    row:        rValidValues;               -- Holds which row the box is in.
    column:     rValidValues;               -- Holds which column the box is in.
    grid:       rValidValues;               -- Holds which grid the box is in.
    given:      Boolean := False;           -- Holds if the boxes value was given or not.
    correct:    Boolean := False;           -- Holds the validity of the boxes value.
  End Record;

  Type rBoxSetup is Array(rBoxIndex) of tBoxStats;


-- |
-- | Definition of Row, Grid, and Column data records and initialization
-- | of each in array form.
-- |

  -- | Row/Column
  Type tRowColumnStats is Record
    low:        Integer;            -- Low index of each row/column of boxes.
    high:       Integer;            -- High index of each row/column of boxes.
    completed:  Boolean := False;   -- Stats if row/column of boxes is filled and valid.
  End Record;

  Type rRowSetup is Array(rValidValues) of tRowColumnStats;
  Type rColumnSetup is Array(rValidValues) of tRowColumnStats;


  -- | Grid
  Type aGridStart is Array(1..3) of Natural;

  Type tGridStats is Record
    start:      aGridStart;         -- What each starting box index is for each row of each grid block.
    completed:  Boolean := False;   -- Stats if grid of boxes is filled and valid.
  End Record;

  Type rGridSetup is Array(rValidValues) of tGridStats;

  aRowData:     rRowSetup;
  aColumnData:  rColumnSetup;
  aGridData:    rGridSetup;
  aBoxData:     rBoxSetup;

  Package stack Is New Stacks (Size => rBoxIndex, Element_Type => Integer);
  sBox : stack.Stack;



  fDebug:       Boolean := False; -- Print out debug information.
  nDifficulty:  rDifficulty; -- Used to case the difficulty instead of strings.
  fSolved:      Boolean := False;

  -- Defines our color setup, including specific modes for text output.
  strClear:     String := ASCII.ESC & "[00m";
  strBold:      String := ASCII.ESC & "[01;37m";

--  nTempMax:     Integer := 1; -- Temp value to check the check row and generation stuff.




-- |
-- | Initialization of starting values in data arrays.
-- |
  Procedure init (aRow: IN OUT rRowSetup;
                  aColumn: IN OUT rColumnSetup;
                  aGrid: IN OUT rGridSetup) is

    nGridIndex: Integer;

  Begin     -- Start the init procedure

    -- Initialize the Row and Column arrays with low and high points.
    For index in rValidValues Loop

      -- Row's init
      aRow(index).low   := (9 * (index - 1)) + 1;
      aRow(index).high  := 9 * index;

      -- Column's init
      aColumn(index).low    := index;
      aColumn(index).high   := (9 * 8) + index;

    End Loop;

    -- Initialize the Grid array with the starting point of each row in that particular grid.
    nGridIndex := 1;
    Declare
      offset: Integer := 1;
    Begin
      Loop
        For i in 1..3 Loop
          For j in offset..(offset + 2) Loop
            aGrid(j).start(i) := nGridIndex;
            nGridIndex := nGridIndex + 3;
          End Loop; -- End j loop
        End Loop; -- End i loop
        offset := offset + 3;
        Exit When offset > 7;
      End Loop; -- End offset loop
    End;


--|
--| If we have supplied the debug flag, then we need to print out the information we just
--| created from our logic structure. This will help in finding out if the values are changing
--| for box locations for an unknown reason.
--|
    If (fDebug) Then

      -- Row Output
      Ada.Text_IO.put_line(strBold & "Row");

      For index in 1..3 Loop
        Case index is
          When 1  =>
            Ada.Text_IO.put("  ");
            For i in rValidValues Loop
              If (i /= 1) Then
                Ada.Text_IO.put(Integer'Image(i) & " ");
              Else
                Ada.Text_IO.put(Integer'Image(i));
              End If;
            End Loop;
            Ada.Text_IO.new_line(1);

          When 2  =>
            Ada.Text_IO.put("l ");
            For i in rValidValues Loop
              Ada.Text_IO.put(Integer'Image(aRow(i).low));
            End Loop;
            Ada.Text_IO.new_line(1);

          When 3  =>
            Ada.Text_IO.put("h ");
            For i in rValidValues Loop
              Ada.Text_IO.put(Integer'Image(aRow(i).high));
            End Loop;
            Ada.Text_IO.new_line(1);
        End Case;
      End Loop;

      Ada.Text_IO.new_line(1);

      -- Column Output
      Ada.Text_IO.put_line("Column");

      For index in 1..3 Loop
        Case index is
          When 1  =>
            Ada.Text_IO.put("  ");
            For i in rValidValues Loop
              Ada.Text_IO.put(Integer'Image(i) & " ");
            End Loop;
            Ada.Text_IO.new_line(1);

          When 2  =>
            Ada.Text_IO.put("l ");
            For i in rValidValues Loop
              Ada.Text_IO.put(Integer'Image(aColumn(i).low) & " ");
            End Loop;
            Ada.Text_IO.new_line(1);

          When 3  =>
            Ada.Text_IO.put("h ");
            For i in rValidValues Loop
              Ada.Text_IO.put(Integer'Image(aColumn(i).high));
            End Loop;
            Ada.Text_IO.new_line(1);
        End Case;
      End Loop;

      --  Ada.Text_IO.put_line(" Row    Column");
      --  Ada.Text_IO.put_line(" l  h   l  h");

      --  Ada.Text_IO.put_line(Integer'Image(aRow(1).low) & " " & Integer'Image(aRow(1).high)
      --    & "  " & Integer'Image(aColumn(1).low) & " " & Integer'Image(aColumn(1).high));

      --  For index in 2..9 Loop
      --    Ada.Text_IO.put_line(Integer'Image(aRow(index).low) & Integer'Image(aRow(index).high)
      --     & " " & Integer'Image(aColumn(index).low) & " " & Integer'Image(aColumn(index).high));
      --  End Loop;


      -- Grid Output
      Ada.Text_IO.new_line(1);
      Ada.Text_IO.put_line("Grid");

      For i in 1..3 Loop
        Ada.Text_IO.put(Integer'Image(i) & "  ");
        For j in 1..3 Loop
          If (j = 1) Then
            Ada.Text_IO.put(Integer'Image(aGrid(i).start(j)) & " ");
          Else
            Ada.Text_IO.put(Integer'Image(aGrid(i).start(j)));
          End If;
        End Loop;
        Ada.Text_IO.new_line(1);
      End Loop;

      For i in 4..9 Loop
        Ada.Text_IO.put(Integer'Image(i) & "  ");
        For j in 1..3 Loop
          Ada.Text_IO.put(Integer'Image(aGrid(i).start(j)));
        End Loop;
        Ada.Text_IO.new_line(1);
      End Loop;

      Ada.Text_IO.put(strClear); -- Return out screen back to the default scheme.

    End If; -- End our debug output for the init procedure.

    -- Initialize the box stack that is used to solve the puzzle.
    stack.initialize(sBox);

  End init; -- End init procedure.





-- |
-- | This function will check for duplication of the given value in that Row.
-- |
  Function checkRow (nValue: Integer;
                     nBox: Integer;
                     aRow: rRowSetup;
                     aBox: rBoxSetup) Return Boolean is

    fResult:  Boolean := True;
    nRow:     rValidValues;

    Function getRow (nBox: IN Integer;
                     aRow: IN rRowSetup) Return rValidValues is

      nResult: Integer;

    Begin

      For index in rValidValues Loop
        If nBox <= aRow(index).high And nBox >= aRow(index).low Then
          nResult := index;
        End If;
      End Loop;

      Return nResult;

    End getRow;

  Begin

    nRow := getRow(nBox, aRow);

    If nRow /= 0 Then
      For index in aRow(nRow).low .. aRow(nRow).high Loop
        If index /= nBox And Then nValue = aBox(index).value Then
          fResult := False;
        End If;
        Exit When Not fResult;
      End Loop;
    End If;

    Return fResult;

  End checkRow;



-- |
-- | This function will check for duplication of the given value in that Column.
-- |
  Function checkColumn (nValue: Integer;
                        nBox: Integer;
                        aColumn: rColumnSetup;
                        aBox: rBoxSetup) Return Boolean is

    fResult:  Boolean := True;
    nColumn:  rValidValues;

    Function getColumn (nBox: IN Integer;
                        aColumn: IN rColumnSetup) Return rValidValues is

      nResult: Integer := 0;

    Begin

      Declare
        nIndex: Integer;
      Begin
        For index in rValidValues Loop
          nIndex := aColumn(index).low;
          Loop
            If nBox = nIndex Then
              nResult := index;
            End If;
            Exit When nResult /= 0 or nIndex = aColumn(index).high;
            nIndex := nIndex + 9;
          End Loop;
        End Loop; -- For index in rValidValues Loop
      End; -- Declare

      Return nResult;

    End getColumn;

  Begin

    nColumn := getColumn(nBox, aColumn);

    If nColumn /= 0 Then
      Declare
        index: Integer := aColumn(nColumn).low;
      Begin
        Loop
          If index /= nBox And Then nValue = aBox(index).value Then
            fResult := False;
          End If;
          Exit When index = aColumn(nColumn).high;
          index := index + 9;
        End Loop;
      End; -- Declare
    End If; -- If nColumn /= 0 Then

    Return fResult;

  End checkColumn;



-- |
-- | This function will check for duplication of the given value in that Grid.
-- |
  Function checkGrid (nValue: Integer;
                      nBox: Integer;
                      aGrid: rGridSetup;
                      aBox: rBoxSetup) Return Boolean is

    fResult:  Boolean := True;
    nGrid:    rValidValues;
    nStart:   Integer;

    Type rGrid is Array(rBoxIndex) of Natural;
    aRebuild: rGrid;

    Function getGrid (nBox: IN Integer;
                      aGrid: IN rGridSetup) Return rValidValues is

      nResult: Integer := 0;

    Begin

      For index in rValidValues Loop
        For i in 1..3 Loop
          nStart := aGrid(index).start(i);
          For j in nStart..nStart+2 Loop
            If nBox = j Then
              nResult := index;
            End If;
            Exit When nResult /= 0;
          End Loop;
          Exit When nResult /= 0;
        End Loop;
        Exit When nResult /= 0;
      End Loop;

      Return nResult;

    End getGrid;

  Begin

    For index in rValidValues Loop
      For i in 1..3 Loop
        nStart := aGrid(index).start(i);
        For j in nStart..nStart+2 Loop
          aRebuild(j) := j;
        End Loop;
      End Loop;
    End Loop;

--    Ada.Text_IO.new_line(1);
--    For index in rBoxIndex Loop
--      If index MOD 9 = 1 Then
--        Ada.Text_IO.new_line(1);
--      End If;
--      Ada.Text_IO.put(Integer'Image(aRebuild(index)) & " ");
--    End Loop;

    nGrid := getGrid(nBox, aGrid);

    If nGrid /= 0 Then
      For i in 1..3 Loop
        nStart := aGrid(nGrid).start(i);
        For j in nStart..nStart+2 Loop
          If nValue = aBox(j).value Then
            fResult := False;
          End If;
          Exit When Not fResult;
        End Loop;
        Exit When Not fResult;
      End Loop;
    End If;

    Return fResult;

  End checkGrid;



-- |
-- | This procedure generates the sudoku puzzle numbers for all levels of difficulty.
-- |
-- g = Generator or Generated value
  Procedure generate (nDifficulty: IN rDifficulty;
                      aRow: IN OUT rRowSetup;
                      aColumn: IN OUT rColumnSetup;
                      aGrid: IN OUT rGridSetup;
                      aBox: IN OUT rBoxSetup) is

    SubType rEasy is Positive Range 20..30;
    SubType rMedium is Positive Range 10..20;
    SubType rHard is Positive Range 1..10;
    SubType rBox is Positive Range 1..81;
    SubType rValue is Natural Range 0..100;

    Package gValue is new Ada.Numerics.Discrete_Random(rValue);
    Package gEasy is new Ada.Numerics.Discrete_Random(rEasy);
    Package gMedium is new Ada.Numerics.Discrete_Random(rMedium);
    Package gHard is new Ada.Numerics.Discrete_Random(rHard);
    Package gBox is new Ada.Numerics.Discrete_Random(rBox);

    hValue:   gValue.Generator;
    hEasy:    gEasy.Generator;
    hMedium:  gMedium.Generator;
    hHard:    gHard.Generator;
    hBox:     gBox.Generator;

    ngValue:  Natural;
    ngBox:    rBox;
    nMax:     Natural;

    fCheckRow:    Boolean := False;
    fCheckColumn: Boolean := False;
    fCheckGrid:   Boolean := False;

  Begin

    gValue.Reset(hValue);
    gBox.Reset(hBox);

    Case nDifficulty is
      When 1 =>
        gEasy.Reset(hEasy);
        nMax := gEasy.Random(hEasy);
      When 2 =>
        gMedium.Reset(hMedium);
        nMax := gMedium.Random(hMedium);
      When 3 =>
        gHard.Reset(hHard);
        nMax := gHard.Random(hHard);
    End Case;

    -- Ada.Text_IO.new_line(1);
    Declare
      index: Integer := 1;
    Begin
      Loop
        -- For index in 1 .. nMax Loop
        Loop
          ngValue := gValue.Random(hValue) MOD 10;
          If ngValue /= 0 Then
            Loop
              ngBox := gBox.Random(hBox);
              Exit When aBox(ngBox).value = 0;
            End Loop;
            fCheckRow     := checkRow(ngValue, ngBox, aRow, aBox);
            fCheckColumn  := checkColumn(ngValue, ngBox, aColumn, aBox);
            fCheckGrid    := checkGrid(ngValue, ngBox, aGrid, aBox);

            -- Ada.Integer_Text_IO.put(ngValue, 2);
            -- Ada.Text_IO.put(" ");
            -- Ada.Integer_Text_IO.put(ngBox, 2);
            -- Ada.Text_IO.new_line(1);
          End If;
          Exit When fCheckRow And fCheckColumn And fCheckGrid;
        End Loop;

        aBox(ngBox).value   := ngValue;
        aBox(ngBox).correct := True;
        aBox(ngBox).given   := True;
        -- Ada.Integer_Text_IO.put(index);
        -- Ada.Integer_Text_IO.put(ngBox, 1);
        -- Ada.Integer_Text_IO.put(aBox(ngBox).value, 2);
        -- Ada.Text_IO.new_line(1);

        Exit When index >= nMax;

        index := index + 1;

        fCheckRow     := False;
        fCheckColumn  := False;
        fCheckGrid    := False;

      End Loop;
    End;

    If (fDebug) Then
      Ada.Text_IO.new_line(1);
      Ada.Text_IO.put("Max Generated: ");
      Ada.Integer_Text_IO.put(nMax);

      Ada.Text_IO.new_line(2);

      Declare
        nIndex:     rValidValues := 1;
        nRowIndex:  Integer := 0;
        fPrinted:   Boolean := False;
      Begin
        For index in rBoxIndex Loop
          Ada.Text_IO.put(Integer'Image(aBox(index).value));
          -- Ada.Integer_Text_IO.put(aBox(index).value, 2);
          If nIndex /= 9 And Then nIndex MOD 3 = 0 Then
            Ada.Text_IO.put(" |");
          End If;

          If nIndex = 9 Then
            Ada.Text_IO.new_line(1);
            nIndex := 1;
            nRowIndex := nRowIndex + 1;
            fPrinted := False;
          Else
            nIndex := nIndex + 1;
          End If;

          If Not fPrinted Then
            If nRowIndex = 3 Or nRowIndex = 6 Then
              Ada.Text_IO.put_line(" ------+-------+------");
              fPrinted := True;
            End If;
          End If;
        End Loop;
      End;

    End If;

  End generate;



  -- Solve the puzzle in it's current state.
  Procedure solve (aBox: IN OUT rBoxSetup;
                  done: OUT Boolean;
                  aRow: IN rRowSetup;
                  aColumn: IN rColumnSetup;
                  aGrid: IN rGridSetup) is

    box   : Integer := 1;
    value : Integer := 1;
    doneV : Boolean := false;
    doneB : Boolean := false;

    Procedure cleanup (start: IN rBoxIndex;
                       aBox : IN OUT rBoxSetup) is

    Begin

      For index in start..81 Loop
        If Not aBox(index).correct Then
          aBox(index).value := 0;
        End If;
      End Loop;

    End cleanup;

  Begin

  While box <= 81 Loop

    doneB := false;

    If aBox(box).value = 0 And Then Not aBox(box).correct Then

      doneV := false;

      While value <= 9 Loop

        If checkRow(value, box, aRow, aBox) And Then checkColumn(value, box, aColumn, aBox) And Then checkGrid(value, box, aGrid, aBox) Then

          aBox(box).value := value;
          stack.push(sBox, box);
          doneV := true;
          value := 1;

          If box + 1 > 81 Then
            doneB := true;
          Else
            box := box + 1;
          End If;

        Else

          If value + 1 > 9 Then

            If Not stack.empty(sBox) Then

              stack.pop(sBox, box);

              If aBox(box).value + 1 > 9 Then

                doneV := true;

              Else

                value := aBox(box).value + 1;

              End If;

              cleanup(box + 1, aBox);

            Else

              If box + 1 > 81 Then

                doneB := true;
                doneV := true;

              Else

                box := box + 1;

              End If;

            End If;

          Else

            value := value + 1;

          End If;

        End If;

        Exit When doneV;

      End Loop;

    Else

      If box + 1 > 81 Then
        doneB := true;
      Else
        box := box + 1;
      End If;

    End If;

    Exit When doneB;

    End Loop;

    -- Check to see if the puzzle has been solved or not.
    done := true;
    For index in rBoxIndex Loop
      If aBox(index).value = 0 Then
        done := false;
        cleanup(1, aBox);
      End If;
      Exit When Not done;
    End Loop;

  End solve;

Begin

  -- Lets check what flags we started with just to make sure that we have all our needed globals defined
  -- incase we are running debug mode with difficulty assigned before hand for debuging purposes.
  Declare
    fDifficulty: Boolean := False;
  Begin
    If Ada.Command_Line.Argument_Count >= 1 Then
      For count in 1..Ada.Command_Line.Argument_Count Loop
        If Ada.Command_Line.Argument(count) = "-debug" Then
          fDebug := True;
        Elsif Not fDifficulty And Then Ada.Command_Line.Argument(count) = "-easy" Then
          nDifficulty := 1;
          fDifficulty := True;
        Elsif Not fDifficulty And Then Ada.Command_Line.Argument(count) = "-medium" Then
          nDifficulty := 2;
          fDifficulty := True;
        Elsif Not fDifficulty And Then Ada.Command_Line.Argument(count) = "-hard" Then
          nDifficulty := 3;
          fDifficulty := True;
        End If;
      End Loop;
    End If;

    If Not fDifficulty Then
      nDifficulty := 1;
    End If;
  End;
  -- End check for startup flags.


  init(aRowData, aColumnData, aGridData);
  generate(nDifficulty, aRowData, aColumnData, aGridData, aBoxData);

  Ada.Text_IO.new_line(2);
  solve(aBoxData, fSolved, aRowData, aColumnData, aGridData);
  If Not fSolved Then
    Ada.Text_IO.put("Could not solve this puzzle.");
  End If;

  If (fDebug) Then

    Ada.Text_IO.new_line(2);

    Declare
      nIndex:     rValidValues := 1;
      nRowIndex:  Integer := 0;
      fPrinted:   Boolean := False;
    Begin
      For index in rBoxIndex Loop
        Ada.Text_IO.put(Integer'Image(aBoxData(index).value));
        -- Ada.Integer_Text_IO.put(aBox(index).value, 2);
        If nIndex /= 9 And Then nIndex MOD 3 = 0 Then
          Ada.Text_IO.put(" |");
        End If;

        If nIndex = 9 Then
          Ada.Text_IO.new_line(1);
          nIndex := 1;
          nRowIndex := nRowIndex + 1;
          fPrinted := False;
        Else
          nIndex := nIndex + 1;
        End If;

        If Not fPrinted Then
          If nRowIndex = 3 Or nRowIndex = 6 Then
            Ada.Text_IO.put_line(" ------+-------+------");
            fPrinted := True;
          End If;
        End If;
      End Loop;
    End;

  End If;

End sudoku;
