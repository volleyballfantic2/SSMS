
/*
Author:		Dale Wood
Version:	12/12/2020
drw  I want to create crud procedures for tables quickly
get, set (save), delete
delete is becoming old school for history and data access (column store) resons. Maybe set a deleted field.

If table has no primary key.  Use identity field to identify rows.
If no identiy or pk field. Use first column or all columns (save signature)?  !!!! Damn it Jim, I'm a DBA not a mind reader !!!!
*/

USE [MovieContext]

DECLARE	@GenerateProcsForTableName varchar(100) = NULL,
			@DatabaseName varchar(100) = NULL,
			@Execute bit = 0,
			@TablePrefix varchar(10) = NULL,
			@SchemaName varchar(20) = NULL,
			@UseSelectWildCard bit = 0

-- which database do we want 
SELECT
	@DatabaseName = ISNULL(@DatabaseName, DB_NAME()) --current db

-- a single table or all tables
SET @GenerateProcsForTableName = ISNULL(@GenerateProcsForTableName, '')

-- do we want to execute them to actually create the procs?
SET @Execute = ISNULL(@Execute, 0)

-- Is there a table name prefix i.e. 'tbl_' we want to remove
SET @TablePrefix = ISNULL(@TablePrefix, '')

-- What schema do you want the stored procedures to be created under?
SET @SchemaName = ISNULL(@SchemaName, 'dbo')

-- do SELECT * or SELECT [ColumnName,]...
SET @UseSelectWildCard = ISNULL(@UseSelectWildCard, 0)

-- ###################################################################################################################################################################################################
DECLARE	@CrLf char(2) = CHAR(13) + CHAR(10), @tab char(1) = CHAR(9)

--hold values from table cursor
DECLARE @CurrentTable varchar(100)

--hold values from column cursor
DECLARE	@ColumnName varchar(100), @DataType varchar(30), @MaxCharLength int, @OrdinalPosition int = 0

--column key info
DECLARE @ConstraintType nvarchar(20)

DECLARE @ObjectName varchar(100) -- this is the tablename with the
DECLARE @ColumnNameCleaned varchar(100)
-- specified tableprefix trim.
DECLARE @TablePrefixLength int = LEN(@TablePrefix)

--store a template for the procedure header
DECLARE @ProcHeader nvarchar(400)
--Procedure sql to run with sp_ExecuteSQL
DECLARE	@SaveProc nvarchar(4000), @SelectProc nvarchar(4000), @DeleteProc nvarchar(4000)

DECLARE	@InsertCols nvarchar(2000), @InsertVals nvarchar(2000)
DECLARE @UpdateSets nvarchar(2000)
DECLARE @SaveSignature nvarchar(2000)
DECLARE @SaveWhere nvarchar(2000)

DECLARE @SelectCols nvarchar(2000)

--Signature to identify a record
DECLARE @PKSignature nvarchar(1000)
DECLARE @PKWhere varchar(1000)

--use as pk signature if no PK in table
DECLARE @IdentitySignature nvarchar(1000)
DECLARE @IdentityWhere varchar(1000)

--use as pk signature if no PK or identity in table
DECLARE @UseFirstColasIdentity bit = 0
DECLARE @FirstSignature nvarchar(1000)
DECLARE @FirstWhere varchar(1000)

--Does 'keyed' pk row exist
DECLARE @Exists nvarchar(500)

--Print 'header' sql before we build precedures
PRINT 'USE [' + @DatabaseName + ']'
PRINT 'GO' + @CrLf
PRINT 'SET ANSI_NULLS ON'
PRINT 'GO' + @CrLf
PRINT 'SET QUOTED_IDENTIFIER ON'
PRINT 'GO' + @CrLf + @CrLf

--declare cursor for tables we want inthe database

DECLARE curTable CURSOR FOR
SELECT
	T.TABLE_NAME
FROM INFORMATION_SCHEMA.Tables t
WHERE t.Table_Catalog = @DatabaseName  --this is set by current db anyway
AND t.TABLE_TYPE = 'BASE TABLE'  --user tables only
AND t.TABLE_SCHEMA = @SchemaName
AND (t.TABLE_NAME = @GenerateProcsForTableName
OR '' = @GenerateProcsForTableName)
ORDER BY T.TABLE_NAME

OPEN curTable

-- get the first row of cursor into variables
FETCH NEXT FROM curTable INTO @CurrentTable
WHILE @@FETCH_STATUS = 0
BEGIN

	SET @ObjectName = @CurrentTable

	IF @TablePrefixLength > 0
	BEGIN
		IF SUBSTRING(@CurrentTable, 1, @TablePrefixLength) = @TablePrefix
		BEGIN
			--PRINT  @CrLf  + 'DEBUG: OBJ NAME: ' + RIGHT(@CurrentTable, LEN(@CurrentTable) - @TablePrefixLength)
			SET @ObjectName = RIGHT(@CurrentTable, LEN(@CurrentTable) - @TablePrefixLength)
		END
	END

	--Generic header
	SET @ProcHeader = 'CREATE OR ALTER PROC [' + @SchemaName + '].[usp_' + @ObjectName + '_<crud>] ('
	SET @ProcHeader = @ProcHeader + '<signature>' + ')' + @CRLF + 'AS' + @CrLf + 'BEGIN' + @CrLf + @tab + 'SET NOCOUNT ON' + @CrLf

	DECLARE curTableCol CURSOR FOR
	SELECT
		P.COLUMN_NAME,
		P.DATA_TYPE,
		P.CHARACTER_MAXIMUM_LENGTH,
		P.ORDINAL_POSITION
	FROM INFORMATION_SCHEMA.Columns P
	WHERE P.TABLE_NAME = @CurrentTable
		AND P.TABLE_SCHEMA = @SchemaName
		AND P.TABLE_CATALOG = @DatabaseName
	ORDER BY P.ORDINAL_POSITION

	-- open the cursor move thru colomns
	OPEN curTableCol

	--New Table init values
	SET @PKWhere = ''
	SET @PKSignature = ''

	SET @IdentityWhere = ''
	SET @IdentitySignature = ''

	SET @FirstWhere = ''
	SET @FirstSignature = ''

	SET @SelectCols = ''
	IF @UseSelectWildcard = 1
		SET @SelectCols = @tab + @tab + ' * ' + @CrLf

	SET @InsertCols = ''
	SET @InsertVals = ''
	SET @UpdateSets = ''
	SET @SaveWhere = ''
	SET @SaveSignature = ''

	-- get the first row of cursor into variables
	FETCH NEXT FROM curTableCol INTO @ColumnName, @DataType, @MaxCharLength, @OrdinalPosition
	WHILE @@FETCH_STATUS = 0
	BEGIN
		SET @ColumnNameCleaned = REPLACE(@ColumnName, ' ', '')

		SET @ConstraintType = ''  --Must reset constrain types as below may (will) return zero rowsfor no constraint a all or no primary key. I used if exists to fix
		IF EXISTS
			(SELECT TOP 1 1
			FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C
			JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE U
				ON U.TABLE_NAME = C.TABLE_NAME
				AND U.CONSTRAINT_CATALOG = C.CONSTRAINT_CATALOG
				AND U.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA
				AND U.CONSTRAINT_NAME = C.CONSTRAINT_NAME
				AND 'PRIMARY KEY' = C.CONSTRAINT_TYPE
			WHERE U.TABLE_NAME = @CurrentTable
				AND U.TABLE_SCHEMA = @SchemaName
				AND U.TABLE_CATALOG = @DatabaseName
				AND U.COLUMN_NAME = @ColumnName)
		SET @ConstraintType = 'PRIMARY KEY' ELSE SET @ConstraintType = ''

		/*
		--Is column primary key?
		select s.name as schemaname, t.name as tablename, c.name as columnname, ic.index_column_id as keycolumnnumber, i.is_primary_key
		from sys.index_columns ic
		inner join sys.columns c on ic.object_id = c.object_id and ic.column_id = c.column_id
		inner join sys.indexes i on ic.object_id = i.object_id and ic.index_id = i.index_id
		inner join sys.tables t on i.object_id = t.object_id
		inner join sys.schemas s on t.schema_id = s.schema_id
		where i.is_primary_key= 1
		*/
		IF @OrdinalPosition = 1 --first column
		BEGIN
			SET @FirstWhere = @FirstWhere + '['+ @ColumnName + '] = @' + REPLACE(@ColumnName, ' ', '') --+ ' AND '

			SET @FirstSignature = @FirstSignature + @tab + '@' + @ColumnNameCleaned + ' ' + @DataType
			IF @DataType IN ('varchar', 'nvarchar', 'char', 'nchar') --add data size/scale to signatue
			BEGIN
				SET @FirstSignature = @FirstSignature + '(' + CAST(@MaxCharLength AS varchar(10)) + ')'
			END
			--SET @FirstSignature = @FirstSignature + ',' + @CrLf
		END

		--Column names for select all columns
		IF @UseSelectWildCard = 0
			SET @SelectCols = @SelectCols + @tab + @tab + '[' + @ColumnName + '],' + @CrLf

		-- is this key colomn?  --for now i only am doing one keyfield but it can be changed
		IF @ConstraintType = 'PRIMARY KEY'
		BEGIN
			SET @PKWhere = @PKWhere + '['+ @ColumnName + '] = @' + REPLACE(@ColumnName, ' ', '') + ' AND '
			
			SET @PKSignature = @PKSignature + @tab + '@' + @ColumnNameCleaned + ' ' + @DataType
			IF @DataType IN ('varchar', 'nvarchar', 'char', 'nchar') --add data size/scale to signatue
			BEGIN
				SET @PKSignature = @PKSignature + '(' + CAST(@MaxCharLength AS varchar(10)) + ')'
			END
			SET @PKSignature = @PKSignature + ',' + @CrLf
		END -- is pk col
		ELSE
		BEGIN --not pk col              
			-- _save --proc add all non pk fields to signature
			IF @DataType NOT IN ('timestamp') --timestamps are auto set, they can not be updated or inserted
			BEGIN
				--we may want identity field in signature as they can be used to get 
				SET @SaveWhere = @SaveWhere + '['+ @ColumnName + '] = @' + REPLACE(@ColumnName, ' ', '') + ' AND '

				SET @SaveSignature = @SaveSignature + @tab + '@' + @ColumnNameCleaned + ' ' + @DataType
				IF @DataType IN ('varchar', 'nvarchar', 'char', 'nchar')
					SET @SaveSignature = @SaveSignature + '(' + CAST(@MaxCharLength AS varchar(10)) + ')'
				SET @SaveSignature = @SaveSignature + ',' + @CrLf

				--is not pk but is identity field
				IF (SELECT COLUMNPROPERTY(OBJECT_ID(@CurrentTable), @ColumnName, 'IsIdentity')) = 1
				BEGIN
					SET @IdentityWhere =  @IdentityWhere + '['+ @ColumnName + '] = @' + REPLACE(@ColumnName, ' ', '') + ' AND '
			
					SET @IdentitySignature = @IdentitySignature + @tab + '@' + @ColumnNameCleaned + ' ' + @DataType
					IF @DataType IN ('varchar', 'nvarchar', 'char', 'nchar') --add data size/scale to signatue
					BEGIN
						SET @IdentitySignature = @IdentitySignature + '(' + CAST(@MaxCharLength AS varchar(10)) + ')'
					END
					SET  @IdentitySignature =  @IdentitySignature + ',' + @CrLf
				END --is identity
			END -- can not insert up update timestamp
		END --not pk

		--technically we can update and save all fields (even pk) except indentity and timestamps.
		IF (SELECT COLUMNPROPERTY(OBJECT_ID(@CurrentTable), @ColumnName, 'IsIdentity')) = 0
			AND  @DataType NOT IN ('timestamp') --timestamps are auto set, they can not be updated or inserted
		BEGIN
			-- INSERT
				SET @InsertCols = @InsertCols + @tab + @tab + @tab + '[' + @ColumnName + '],' + @CrLf
				SET @InsertVals = @InsertVals + @tab + @tab + @tab + '@' + @ColumnNameCleaned + ',' + @CrLf
			-- UPDATE 
				SET @UpdateSets = @UpdateSets + @tab + @tab + @tab + '[' + @ColumnName + '] = @' + @ColumnNameCleaned + ',' + @CrLf
				--update only if input is not null; useful when not a web application post call
				--UPDATE field = COALESCE(@var, field, default) 
				--SET @UpdateSets = @UpdateSets + @tab + @tab + @tab + '[' + @ColumnName + '] = COALESCE(@' + @ColumnNameCleaned + ', [' + @ColumnName + ']),' + @CrLf
				--UPDATE field = ISNULL(@var, field) 
				--SET @UpdateSets = @UpdateSets + @tab + @tab + @tab + '[' + @ColumnName + '] = ISNULL(@' + @ColumnNameCleaned + ', [' + @ColumnName + ']),' + @CrLf
		END --is not identiy

		-- fetch next row of cursor into variables
		FETCH NEXT FROM curTableCol INTO @ColumnName, @DataType, @MaxCharLength, @OrdinalPosition
	END  --not pk col

	-- ----------------
	-- clean up cursor
	-- ----------------
	CLOSE curTableCol
	DEALLOCATE curTableCol

	-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	--PK where remove extra trailing ' AND '
	IF LEN(@PKWhere) > 3
		SET @PKWhere = SUBSTRING(@PKWhere, 0, LEN(@PKWhere) - 3)

	--PK signature remove extra trailing  ',' + @CrLf
	IF LEN(@PKSignature) > 2
		SET @PKSignature = SUBSTRING(@PKSignature, 0, LEN(@PKSignature) - 2)
		
	IF LEN(@IdentityWhere) > 3
		SET @IdentityWhere = SUBSTRING(@IdentityWhere, 0, LEN(@IdentityWhere) - 3)

	IF LEN(@IdentitySignature) > 2
		SET @IdentitySignature = SUBSTRING(@IdentitySignature, 0, LEN(@IdentitySignature) - 2)

	IF LEN(@SaveWhere) > 3 
		SET @SaveWhere = SUBSTRING(@SaveWhere, 0, LEN(@SaveWhere) - 2)
		
	IF LEN(@SaveSignature) > 2 
		SET @SaveSignature = SUBSTRING(@SaveSignature, 0, LEN(@SaveSignature) - 2)

	--if no PK data to select rows use identity
	--use all cols when no pks and no identiy -  @SaveSignature @SaveWhere
	IF LEN(REPLACE(@PKWhere, ' ', '')) < 1
	BEGIN
		IF LEN(REPLACE(@IdentityWhere, ' ', '')) > 1
		BEGIN
			SET @PKWhere = @IdentityWhere
			SET @PKSignature = @IdentitySignature
		END
		ELSE 	--if no identiy data to select rows use first column or all columns
		BEGIN
			IF  @UseFirstColasIdentity = 0
			BEGIN 
				--all columns
				SET @PKWhere = @SaveWhere
				SET @PKSignature = @SaveSignature
			END
			ELSE
			BEGIN
				--first column
				SET @PKWhere = @FirstWhere
				SET @PKSignature = @FirstSignature
			END
		END
	END

	DECLARE @FirstPk nvarchar(100)

	SET  @FirstPk = right(@PKSignature, len(@PKSignature) + 1 - charindex('@', @PKSignature))
	SET  @FirstPk = LEFT(@FirstPk, CHARINDEX(' ', @FirstPk) - 1)

	-- SELECT
	IF LEN(@SelectCols) > 2
		SET @SelectCols = SUBSTRING(@SelectCols, 0, LEN(@SelectCols) - 2)  --',' + @CrLf

	-- UPDATE (remove trailing comma and append the WHERE clause)
	IF LEN(@UpdateSets) > 2
		SET @UpdateSets = SUBSTRING(@UpdateSets, 0, LEN(@UpdateSets) - 2)  --',' + @CrLf
	
	IF LEN(@InsertCols) > 2
		SET @InsertCols = SUBSTRING(@InsertCols, 0, LEN(@InsertCols) - 2) --',' + @CrLf
	
	IF LEN(@InsertVals) > 2
		SET @InsertVals = SUBSTRING(@InsertVals, 0, LEN(@InsertVals) - 2) --',' + @CrLf

	SET @Exists = 'EXISTS (SELECT 1 FROM [' + @SchemaName + '].[' + @CurrentTable + '] WITH(NOLOCK) WHERE ' +  @PKWhere + ')'
	/*
		These are single change transactions so I am not setting rollbacks
		BEGIN TRAN @TranName; COMMIT TRAN @TranName; ROLLBACK TRAN @TranName;

		TRY CATCH is useful to set flags 
		--TODO!
		Should I use RETURN @ReturnVal vs SELECT @ReturnVal AS ReturnVal
	*/

	--*********************************************************************************************************************************************--
	-- INSERT / UPDATE
	SELECT
		@SaveProc = REPLACE(@ProcHeader, '<crud>', 'Save')
	SELECT
		@SaveProc = REPLACE(@SaveProc, '<signature>', @PKSignature + '<signature>')

	--maybe declare @ReturnVal int = 0
	IF LEN(REPLACE(@SaveSignature, ' ', '')) > 1
		SELECT @SaveProc = REPLACE(@SaveProc, '<signature>', ',' + @CrLf + @SaveSignature + ',' + @CrLf + '@ReturnVal int out')
	ELSE
		SELECT @SaveProc = REPLACE(@SaveProc, '<signature>', ',' + @CrLf + '@ReturnVal int out')

	SET @SaveProc = @SaveProc + @tab + 'IF ' + @Exists + +@CrLf --update
	SET @SaveProc = @SaveProc + @tab + 'BEGIN' + @CrLf
	--we could use update COALESCE select olds and use them is input is null --default to original field value 
	--SET field1 = ISNULL(@var1, field1)
	SET @SaveProc = @SaveProc + @tab + @tab + 'UPDATE ' + @CurrentTable + ' SET ' + @CrLf + @UpdateSets + @CrLf + @tab + @tab + 'WHERE '  + @PKWhere + @CrLf
	SET @SaveProc = @SaveProc + @tab + @tab + 'SET @ReturnVal = ' + @FirstPk + @CrLf
	SET @SaveProc = @SaveProc + @tab + 'END' + @CrLf
	SET @SaveProc = @SaveProc + @tab + 'ELSE' + @CrLf  --insert
	SET @SaveProc = @SaveProc + @tab + 'BEGIN' + @CrLf
	--could add if isnull(@PKWhere, 0) = 0  dothis insert else error
	SET @SaveProc = @SaveProc + @tab + @tab + 'INSERT INTO [' + @SchemaName + '].[' + @CurrentTable + '] (' + @CrLf + @InsertCols + @CrLf
	SET @SaveProc = @SaveProc + @tab + @tab + ')' + @CrLf
	SET @SaveProc = @SaveProc + @tab + @tab + 'VALUES (' + @CrLf + @InsertVals + @CrLf + @tab + @tab + ')' + @CrLf
	SET @SaveProc = @SaveProc + @tab + @tab + 'SET @ReturnVal = SCOPE_IDENTITY()' + @CrLf
	SET @SaveProc = @SaveProc + @tab + 'END' + @CrLf
	SET @SaveProc = @SaveProc + @tab + 'SELECT @ReturnVal AS ReturnVal' + @CrLf
--	SET @SaveProc = @SaveProc + @tab + 'SET NOCOUNT OFF' + @CrLf
	SET @SaveProc = @SaveProc + 'END' + @CrLf

	-- SELECT
	SELECT
		@SelectProc = REPLACE(@ProcHeader, '<crud>', 'Get')
	SELECT
		@SelectProc = REPLACE(@SelectProc, '<signature>', @PKSignature  + ',' + @CrLf + '@ReturnVal int out')

	SET @SelectProc = @SelectProc + @tab + 'IF ' + @Exists + @crlf --select
	SET @SelectProc = @SelectProc + @tab + @tab + 'SELECT ' + @SelectCols + @CrLf
	SET @SelectProc = @SelectProc + @tab + @tab + 'FROM [' + @SchemaName + '].[' + @CurrentTable + '] WITH(NOLOCK)' + @CrLf
	SET @SelectProc = @SelectProc + @tab + @tab + 'WHERE ' + @PKWhere + @CrLf
	SET @SelectProc = @SelectProc + @tab + 'ELSE' + @CrLf  --select 'all'
	--could add if isnull(@PKWhere, 0) = 0  dothis select all else error
	SET @SelectProc = @SelectProc + @tab + @tab + 'SELECT TOP 65000 ' + @SelectCols + @CrLf + @tab + @tab 
		+ 'FROM [' + @SchemaName + '].[' + @CurrentTable + '] WITH(NOLOCK)' + @CrLf + @tab + @tab + 'ORDER BY 1 DESC' + @CrLf --desc to get most current
	SET @SelectProc = @SelectProc + @tab + @tab + 'SET @ReturnVal = ' + @FirstPk + @CrLf
	--NOT IN RETURN FROM SELECT!!!   --SET @SelectProc = @SelectProc + @tab + 'SELECT @ReturnVal AS ReturnVal' + @CrLf
--	SET @SelectProc = @SelectProc + @tab + 'SET NOCOUNT OFF' + @CrLf
	SET @SelectProc = @SelectProc + 'END' + @CrLf

	-- DELETE
	SELECT
		@DeleteProc = REPLACE(@ProcHeader, '<crud>', 'Delete')
	SELECT
	--DRW USe exists?
		@DeleteProc = REPLACE(@DeleteProc, '<signature>', @PKSignature  + ',' + @CrLf + '@ReturnVal int out')
	--could add if exists do this delete else error
	SET @DeleteProc = @DeleteProc +@tab + 'DELETE FROM [' + @SchemaName + '].[' + @CurrentTable + ']' + @CrLf
	SET @DeleteProc = @DeleteProc + @tab + @tab + 'WHERE ' +  @PKWhere + @CrLf
	SET @DeleteProc = @DeleteProc + @tab + @tab + 'SET @ReturnVal = ' + @FirstPk + @CrLf
--	SET @DeleteProc = @DeleteProc + @tab + @tab + 'SET @ReturnVal = @@ROWCOUNT' + @CrLf
	SET @DeleteProc = @DeleteProc + @tab + 'SELECT @ReturnVal AS ReturnVal' + @CrLf
--	SET @DeleteProc = @DeleteProc + @tab + 'SET NOCOUNT OFF' + @CrLf
	SET @DeleteProc = @DeleteProc + 'END' + @CrLf

	--PRINT 'PKs - ' + @PKSignature
	--PRINT 'Sig - ' + @SaveSignature

	PRINT @SaveProc + 'GO' + @CrLf + @CrLf
	PRINT @SelectProc + 'GO' + @CrLf + @CrLf
	PRINT @DeleteProc + 'GO' + @CrLf + @CrLf
	IF @Execute = 1
	BEGIN
		EXEC sp_Executesql @SaveProc
		EXEC sp_Executesql @SelectProc
		EXEC sp_Executesql @DeleteProc
	END
	--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	FETCH NEXT FROM curTable INTO @CurrentTable

END -- while

CLOSE curTable
DEALLOCATE curTable