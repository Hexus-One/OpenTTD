#include "stdafx.h"
#include "ttd.h"
#include "window.h"
#include "string.h"
#include "table/strings.h"
#include "hal.h"

#include <direct.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <time.h>
#include <dos.h>

#define INCL_DOS
#define INCL_OS2MM
#define INCL_WIN

#include <os2.h>
#include <os2me.h>

#if defined(WITH_SDL)
#include <SDL.h>
#endif

static inline int strcasecmp(const char* s1, const char* s2)
{
	return stricmp(s1, s2);
}

static char *_fios_path;
static char *_fios_save_path;
static char *_fios_scn_path;
static FiosItem *_fios_items;
static int _fios_count, _fios_alloc;

static FiosItem *FiosAlloc(void)
{
	if (_fios_count == _fios_alloc) {
		_fios_alloc += 256;
		_fios_items = realloc(_fios_items, _fios_alloc * sizeof(FiosItem));
	}
	return &_fios_items[_fios_count++];
}

int compare_FiosItems(const void *a, const void *b)
{
	const FiosItem *da = (const FiosItem *)a;
	const FiosItem *db = (const FiosItem *)b;
	int r;

	if (_savegame_sort_order < 2) // sort by date
		r = da->mtime < db->mtime ? -1 : 1;
	else
		r = strcasecmp(
			da->title[0] != '\0' ? da->title : da->name,
			db->title[0] != '\0' ? db->title : db->name
		);

	if (_savegame_sort_order & 1) r = -r;
	return r;
}


static DIR *my_opendir(char *path, char *file)
{
	char paths[MAX_PATH];

	append_path(paths, path, file);
	return opendir(paths);
}

static void append_path(char *out, const char *path, const char *file)
{
	if (path[2] == '\\' && path[3] == '\0')
		sprintf(out, "%s%s", path, file);
	else
		sprintf(out, "%s\\%s", path, file);
}

// Get a list of savegames
FiosItem *FiosGetSavegameList(int *num, int mode)
{
	FiosItem *fios;
	DIR *dir;
	struct dirent *dirent;
	struct stat sb;
	int sort_start;
	char filename[MAX_PATH];

	if (_fios_save_path == NULL) {
		_fios_save_path = malloc(MAX_PATH);
		strcpy(_fios_save_path, _path.save_dir);
	}

	if (_game_mode == GM_EDITOR)
		_fios_path = _fios_scn_path;
	else
		_fios_path = _fios_save_path;

	// Parent directory, only if not of the type C:\.
	if (_fios_path[3] != '\0') {
		fios = FiosAlloc();
		fios->type = FIOS_TYPE_PARENT;
		fios->mtime = 0;
		strcpy(fios->title, ".. (Parent directory)");
	}

	// Show subdirectories first
	dir = my_opendir(_fios_path, "*.*");
	if (dir != NULL) {
		while ((dirent = readdir(dir)) != NULL) {
			append_path(filename, _fios_path, dirent->d_name);
			if (!stat(filename, &sb) && S_ISDIR(sb.st_mode) &&
					strcmp(filename, ".") != 0 &&
					strcmp(filename, "..") != 0) {
				fios = FiosAlloc();
				fios->type = FIOS_TYPE_DIR;
				fios->mtime = 0;
				ttd_strlcpy(fios->name, dirent->d_name, lengthof(fios->name));
				snprintf(fios->title, lengthof(fios->title),
					"%s\\ (Directory)", dirent->d_name);
			}
		}
		closedir(dir);
	}

	{
		/* XXX ugly global variables ... */
		byte order = _savegame_sort_order;
		_savegame_sort_order = 2; // sort ascending by name
		qsort(_fios_items, _fios_count, sizeof(FiosItem), compare_FiosItems);
		_savegame_sort_order = order;
	}

	// this is where to start sorting
	sort_start = _fios_count;

	/* Show savegame files
	 * .SAV OpenTTD saved game
	 * .SS1 Transport Tycoon Deluxe preset game
	 * .SV1 Transport Tycoon Deluxe (Patch) saved game
	 * .SV2 Transport Tycoon Deluxe (Patch) saved 2-player game
	 */
	dir = my_opendir(_fios_path, "*.*");
	if (dir != NULL) {
		while ((dirent = readdir(dir)) != NULL) {
			char *t;

			append_path(filename, _fios_path, dirent->d_name);
			if (stat(filename, &sb) || S_ISDIR(sb.st_mode)) continue;

			t = strrchr(dirent->d_name, '.');
			if (t != NULL && strcasecmp(t, ".sav") == 0) { // OpenTTD
				fios = FiosAlloc();
				fios->type = FIOS_TYPE_FILE;
				fios->mtime = sb.st_mtime;
				fios->title[0] = '\0';
				ttd_strlcpy(fios->name, dirent->d_name, lengthof(fios->name));
			} else if (mode == SLD_LOAD_GAME || mode == SLD_LOAD_SCENARIO) {
				if (t != NULL && (
							strcasecmp(t, ".ss1") == 0 ||
							strcasecmp(t, ".sv1") == 0 ||
							strcasecmp(t, ".sv2") == 0
						)) { // TTDLX(Patch)
					fios = FiosAlloc();
					fios->type = FIOS_TYPE_OLDFILE;
					fios->mtime = sb.st_mtime;
					ttd_strlcpy(fios->name, dirent->d_name, lengthof(fios->name));
					GetOldSaveGameName(fios->title, filename);
				}
			}
		}
		closedir(dir);
	}

	qsort(_fios_items + sort_start, _fios_count - sort_start, sizeof(FiosItem), compare_FiosItems);

	// Drives
	{
		uint save;
		uint disk;
		uint total;

		/* save original drive */
		_dos_getdrive(&save);

		/* get available drive letters */
		for (disk = 1; disk < 27; ++disk)
		{
			uint disk2;

			_dos_setdrive(disk, &total);
			_dos_getdrive(&disk2);

			if (disk == disk2)
			{
				fios = FiosAlloc();
				fios->type = FIOS_TYPE_DRIVE;
				sprintf(fios->title, "%c:", 'A' + disk - 1);
			}
		}

		_dos_setdrive(save, &total);
	}

	*num = _fios_count;
	return _fios_items;
}

// Get a list of scenarios
FiosItem *FiosGetScenarioList(int *num, int mode)
{
	FiosItem *fios;
	DIR *dir;
	struct dirent *dirent;
	struct stat sb;
	int sort_start;
	char filename[MAX_PATH];

	if (mode == SLD_NEW_GAME || _fios_scn_path == NULL) {
		if (_fios_scn_path == NULL)
			_fios_scn_path = malloc(MAX_PATH);
		strcpy(_fios_scn_path, _path.scenario_dir);
	}

	_fios_path = _fios_scn_path;

	// Parent directory, only if not of the type C:\.
	if (_fios_path[3] != '\0' && mode != SLD_NEW_GAME) {
		fios = FiosAlloc();
		fios->type = FIOS_TYPE_PARENT;
		fios->mtime = 0;
		strcpy(fios->title, ".. (Parent directory)");
	}

	// Show subdirectories first
	dir = my_opendir(_fios_path, "*.*");
	if (dir != NULL) {
		while ((dirent = readdir(dir)) != NULL) {
			append_path(filename, _fios_path, dirent->d_name);
			if (!stat(filename, &sb) && S_ISDIR(sb.st_mode) &&
					strcmp(filename, ".") != 0 &&
					strcmp(filename, "..") != 0) {
				fios = FiosAlloc();
				fios->type = FIOS_TYPE_DIR;
				fios->mtime = 0;
				ttd_strlcpy(fios->name, dirent->d_name, lengthof(fios->name));
				snprintf(fios->title, lengthof(fios->title),
					"%s\\ (Directory)", dirent->d_name);
			}
		}
		closedir(dir);
	}

	// this is where to start sorting
	sort_start = _fios_count;

	/* Show scenario files
	 * .SCN OpenTTD style scenario file
	 * .SV0 Transport Tycoon Deluxe (Patch) scenario
	 * .SS0 Transport Tycoon Deluxe preset scenario
	 */
	dir = my_opendir(_fios_path, "*.*");
	if (dir != NULL) {
		while ((dirent = readdir(dir)) != NULL) {
			char *t;

			append_path(filename, _fios_path, dirent->d_name);
			if (stat(filename, &sb) || S_ISDIR(sb.st_mode)) continue;

			t = strrchr(dirent->d_name, '.');
			if (t != NULL && strcasecmp(t, ".scn") == 0) { // OpenTTD
				fios = FiosAlloc();
				fios->type = FIOS_TYPE_SCENARIO;
				fios->mtime = sb.st_mtime;
				fios->title[0] = '\0';
				ttd_strlcpy(fios->name, dirent->d_name, lengthof(fios->name));
			} else if (mode == SLD_LOAD_GAME || mode == SLD_LOAD_SCENARIO ||
					mode == SLD_NEW_GAME) {
				if (t != NULL && (
							strcasecmp(t, ".sv0") == 0 ||
							strcasecmp(t, ".ss0") == 0
						)) { // TTDLX(Patch)
					fios = FiosAlloc();
					fios->type = FIOS_TYPE_OLD_SCENARIO;
					fios->mtime = sb.st_mtime;
					GetOldScenarioGameName(fios->title, filename);
					ttd_strlcpy(fios->name, dirent->d_name, lengthof(fios->name));
				}
			}
		}
		closedir(dir);
	}

	qsort(_fios_items + sort_start, _fios_count - sort_start, sizeof(FiosItem), compare_FiosItems);

	// Drives
	if (mode != SLD_NEW_GAME)
	{
		unsigned save, disk, disk2, total;

		/* save original drive */
		_dos_getdrive(&save);

		/* get available drive letters */

		for (disk = 1; disk < 27; ++disk)
		{
			_dos_setdrive(disk, &total);
			_dos_getdrive(&disk2);

			if (disk == disk2)
			{
				fios = FiosAlloc();
				fios->type = FIOS_TYPE_DRIVE;
				fios->title[0] = disk + 'A'-1;
				fios->title[1] = ':';
				fios->title[2] = 0;
			}
		}

		_dos_setdrive(save, &total);
	}

	*num = _fios_count;
	return _fios_items;
}


// Free the list of savegames
void FiosFreeSavegameList(void)
{
	free(_fios_items);
	_fios_items = NULL;
	_fios_alloc = _fios_count = 0;
}

// Browse to
char *FiosBrowseTo(const FiosItem *item)
{
	char *path = _fios_path;
	char *s;

	switch (item->type) {
		case FIOS_TYPE_DRIVE:
			sprintf(path, "%c:\\", item->title[0]);
			break;

		case FIOS_TYPE_PARENT:
			s = strrchr(path, '\\');
			if (s != NULL) *s = '\0';
			break;

		case FIOS_TYPE_DIR:
			s = strchr(item->name, '\\');
			if (s != NULL) *s = '\0';
			strcat(path, "\\");
			strcat(path, item->name);
			break;

		case FIOS_TYPE_FILE:
		case FIOS_TYPE_OLDFILE:
		case FIOS_TYPE_SCENARIO:
		case FIOS_TYPE_OLD_SCENARIO: {
			static char str_buffr[512];

			sprintf(str_buffr, "%s\\%s", path, item->name);
			return str_buffr;
		}
	}

	return NULL;
}

// Get descriptive texts.
// Returns a path as well as a
//  string describing the path.
StringID FiosGetDescText(const char **path)
{
	struct diskfree_t free;
	char drive;

	*path = _fios_path;
	drive = *path[0] - 'A' + 1;

	_getdiskfree(drive, &free);

	SetDParam(0, free.avail_clusters * free.sectors_per_cluster * free.bytes_per_sector);
	return STR_4005_BYTES_FREE;
}

void FiosMakeSavegameName(char *buf, const char *name)
{
	if (_game_mode == GM_EDITOR)
		sprintf(buf, "%s\\%s.scn", _fios_path, name);
	else
		sprintf(buf, "%s\\%s.sav", _fios_path, name);
}

void FiosDelete(const char *name)
{
	char path[512];

	snprintf(path, lengthof(path), "%s\\%s", _fios_path, name);
	unlink(path);
}

const DriverDesc _video_driver_descs[] = {
	{	"null",			"Null Video Driver",		&_null_video_driver,		0},
#if defined(WITH_SDL)
	{	"sdl",			"SDL Video Driver",			&_sdl_video_driver,			1},
#endif
	{	"dedicated",	"Dedicated Video Driver",	&_dedicated_video_driver,	0},
	{	NULL,			NULL,						NULL,						0}
};

const DriverDesc _sound_driver_descs[] = {
	{	"null",	"Null Sound Driver",	&_null_sound_driver,		0},
#if defined(WITH_SDL)
	{	"sdl",	"SDL Sound Driver",		&_sdl_sound_driver,			1},
#endif
	{	NULL,	NULL,					NULL,						0}
};

const DriverDesc _music_driver_descs[] = {
	{	"os2",		"OS/2 Music Driver",		&_os2_music_driver,			0},
	{   "null",     "Null Music Driver",	    &_null_music_driver,	    1},
	{	NULL,		NULL,						NULL,						0}
};

/* GetOSVersion returns the minimal required version of OS to be able to use that driver.
	 Not needed for OS/2. */
byte GetOSVersion(void)
{
	return 2;  // any arbitrary number bigger then 0
}

bool FileExists(const char *filename)
{
	return access(filename, 0) == 0;
}

static int LanguageCompareFunc(const void *a, const void *b)
{
	return strcmp(*(const char* const *)a, *(const char* const *)b);
}

int GetLanguageList(char **languages, int max)
{
	DIR *dir;
	struct dirent *dirent;
	int num = 0;

	dir = opendir(_path.lang_dir);
	if (dir != NULL) {
		while ((dirent = readdir(dir))) {
			char *t = strrchr(dirent->d_name, '.');
			if (t && !strcmp(t, ".lng")) {
				languages[num++] = strdup(dirent->d_name);
				if (num == max) break;
			}
		}
		closedir(dir);
	}

	qsort(languages, num, sizeof(char*), LanguageCompareFunc);
	return num;
}

static void ChangeWorkingDirectory(char *exe)
{
	char *s = strrchr(exe, '\\');
	if (s != NULL) {
		*s = '\0';
		chdir(exe);
		*s = '\\';
	}
}

void ShowInfo(const char *str)
{
	HAB hab;
	HMQ hmq;
	ULONG rc;

	// init PM env.
	hmq = WinCreateMsgQueue((hab = WinInitialize(0)), 0);

	// display the box
	rc = WinMessageBox(HWND_DESKTOP, HWND_DESKTOP, str, "OpenTTD", 0, MB_OK | MB_MOVEABLE | MB_INFORMATION);

	// terminate PM env.
	WinDestroyMsgQueue(hmq);
	WinTerminate(hab);
}

void ShowOSErrorBox(const char *buf)
{
	HAB hab;
	HMQ hmq;
	ULONG rc;

	// init PM env.
	hmq = WinCreateMsgQueue((hab = WinInitialize(0)), 0);

	// display the box
	rc = WinMessageBox(HWND_DESKTOP, HWND_DESKTOP, buf, "OpenTTD", 0, MB_OK | MB_MOVEABLE | MB_ERROR);

	// terminate PM env.
	WinDestroyMsgQueue(hmq);
	WinTerminate(hab);
}

int CDECL main(int argc, char* argv[])
{
	// change the working directory to enable doubleclicking in UIs
	ChangeWorkingDirectory(argv[0]);

	_random_seeds[0][1] = _random_seeds[0][0] = time(NULL);


	return ttd_main(argc, argv);
}

void DeterminePaths(void)
{
	char *s;

	_path.game_data_dir = malloc(MAX_PATH);
	ttd_strlcpy(_path.game_data_dir, GAME_DATA_DIR, MAX_PATH);
	#if defined SECOND_DATA_DIR
	_path.second_data_dir = malloc(MAX_PATH);
	ttd_strlcpy(_path.second_data_dir, SECOND_DATA_DIR, MAX_PATH);
	#endif

#if defined(USE_HOMEDIR)
	{
		const char *homedir = getenv("HOME");

		if (homedir == NULL) {
			const struct passwd *pw = getpwuid(getuid());
			if (pw != NULL) homedir = pw->pw_dir;
		}

		_path.personal_dir = str_fmt("%s" PATHSEP "%s", homedir, PERSONAL_DIR);
	}

#else /* not defined(USE_HOMEDIR) */

	_path.personal_dir = malloc(MAX_PATH);
	ttd_strlcpy(_path.personal_dir, PERSONAL_DIR, MAX_PATH);

	// check if absolute or relative path
	s = strchr(_path.personal_dir, '\\');

	// add absolute path
	if (s == NULL || _path.personal_dir != s) {
		getcwd(_path.personal_dir, MAX_PATH);
		s = strchr(_path.personal_dir, 0);
		*s++ = '\\';
		ttd_strlcpy(s, PERSONAL_DIR, MAX_PATH);
	}

#endif /* defined(USE_HOMEDIR) */

	s = strchr(_path.personal_dir, 0);

	// append a / ?
	if (s[-1] != '\\') strcpy(s, "\\");

	_path.save_dir = str_fmt("%ssave", _path.personal_dir);
	_path.autosave_dir = str_fmt("%s\\autosave", _path.save_dir);
	_path.scenario_dir = str_fmt("%sscenario", _path.personal_dir);
	_path.gm_dir = str_fmt("%sgm\\", _path.game_data_dir);
	_path.data_dir = str_fmt("%sdata\\", _path.game_data_dir);

	if (_config_file == NULL)
		_config_file = str_fmt("%sopenttd.cfg", _path.personal_dir);

	_highscore_file = str_fmt("%shs.dat", _path.personal_dir);
	_log_file = str_fmt("%sopenttd.log", _path.personal_dir);

#if defined CUSTOM_LANG_DIR
	// sets the search path for lng files to the custom one
	_path.lang_dir = malloc( MAX_PATH );
	ttd_strlcpy( _path.lang_dir, CUSTOM_LANG_DIR, MAX_PATH);
#else
	_path.lang_dir = str_fmt("%slang\\", _path.game_data_dir);
#endif

	// create necessary folders
	mkdir(_path.personal_dir);
	mkdir(_path.save_dir);
	mkdir(_path.autosave_dir);
	mkdir(_path.scenario_dir);
}

// FUNCTION: OS2_SwitchToConsoleMode
//
// Switches OpenTTD to a console app at run-time, instead of a PM app
// Necessary to see stdout, etc

void OS2_SwitchToConsoleMode(void)
{
	PPIB pib;
	PTIB tib;

	DosGetInfoBlocks(&tib, &pib);

	// Change flag from PM to VIO
	pib->pib_ultype = 3;
}

/**********************
 * OS/2 MIDI PLAYER
 **********************/

/* Interesting how similar the MCI API in OS/2 is to the Win32 MCI API,
 * eh? Anyone would think they both came from the same place originally! ;)
 */

static long CDECL MidiSendCommand(const char *cmd, ...)
{
	va_list va;
	char buf[512];
	va_start(va, cmd);
	vsprintf(buf, cmd, va);
	va_end(va);
	return mciSendString(buf, NULL, 0, NULL, 0);
}

static void OS2MidiPlaySong(const char *filename)
{
	MidiSendCommand("close all");

	if (MidiSendCommand("open %s type sequencer alias song", filename) != 0)
		return;

	MidiSendCommand("play song from 0");
}

static void OS2MidiStopSong(void)
{
	MidiSendCommand("close all");
}

static void OS2MidiSetVolume(byte vol)
{
	MidiSendCommand("set song audio volume %d", ((vol/127)*100));
}

static bool OS2MidiIsSongPlaying(void)
{
	char buf[16];
	mciSendString("status song mode", buf, sizeof(buf), NULL, 0);
	return strcmp(buf, "playing") == 0 || strcmp(buf, "seeking") == 0;
}

static const char *OS2MidiStart(const char * const *parm)
{
	return 0;
}

static void OS2MidiStop(void)
{
	MidiSendCommand("close all");
}

const HalMusicDriver _os2_music_driver = {
	OS2MidiStart,
	OS2MidiStop,
	OS2MidiPlaySong,
	OS2MidiStopSong,
	OS2MidiIsSongPlaying,
	OS2MidiSetVolume,
};

bool InsertTextBufferClipboard(Textbuf *tb)
{
	// TODO
	return false;
}
