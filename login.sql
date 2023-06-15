-- -----------------------------------------------------------------------------
--
--       Origins: LHS Telekom GmbH & Co KG
--
--        Author: R&D Database Team, Dreieich
--
--          Date: 12-Jul-2004
--
--       Version: "@(#) ./lhsj_main/bscs/dmf/resource/setup/login.sql, , BSCSIX_16.0_GA, BSCSIX_16.0_GA_151205 65151e2151680dd4b5e273ad65cfe1aa26546d92 4-Dec-2015"
--
--       Purpose: Set your SQLPLUS environment.
--                This file hase to be called in every SQLPLUS login
--                before any migration script is executed.
--
-- -----------------------------------------------------------------------------
--
--  Copyright (c) 2004 LHS Telekom GmbH & Co. KG
--
--  The copyright in this work is vested in LHS.
--  The information contained in this work (either in whole or in part)
--  is confidential and must not be modified, reproduced, disclosed or
--  disseminated to others or used for purposes other than that for which
--  it is supplied, without the prior written permission of LHS.
--  If this work or any part hereof is furnished to a third party by
--  virtue of a contract with that party, use of this work by such party
--  shall be governed by the express contractual terms between LHS, which
--  is party to that contract and the said party.
--
--  The information in this document is subject to change without notice
--  and should not be construed as a commitment by LHS. LHS assumes no
--  responsibility for any errors that may appear in this document. With
--  the appearance of a new version of this document all older versions
--  become invalid.
--
--  All rights reserved
--
-- -----------------------------------------------------------------------------
/*
   Include sqlplus settings and parameters.
*/

set linesize 300
set serveroutput on
set time on
set trimout on
set trimspool on
set pagesize 50000
set arraysize 1
set echo on

-- --------------------------------------------------
-- Define column length of CHAR and VARCHAR2 columns in "CHAR"
-- removed with PN 00408316/d
-- ALTER SESSION SET NLS_LENGTH_SEMANTICS='CHAR';

-- --------------------------------------------------
-- Set the character used to prefix substitution variables.
-- The setting below overrides the Oracle default (&).
set define '~'

-- --------------------------------------------------
-- Define column alias for the storage parameters
define PARA_STORAGE_CLAUSE = ''
COLUMN PARA_STORAGE_CLAUSE NEW_VALUE PARA_STORAGE_CLAUSE NOPRINT

-- --------------------------------------------------
set serveroutput on size 1000000;
