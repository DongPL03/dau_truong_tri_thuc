-- Migration: Remove COURSE_AND_PRACTICE from loai_su_dung ENUM
-- Date: 2025-12-23

-- Step 1: Update existing data from COURSE_AND_PRACTICE to COURSE_ONLY
-- (Assuming we want to keep them as course-only sets)
UPDATE bo_cau_hoi 
SET loai_su_dung = 'COURSE_ONLY' 
WHERE loai_su_dung = 'COURSE_AND_PRACTICE';

-- Step 2: Modify ENUM to remove COURSE_AND_PRACTICE
ALTER TABLE bo_cau_hoi 
MODIFY COLUMN loai_su_dung ENUM('PRACTICE_ONLY','RANKED_ONLY','CASUAL_ONLY','COURSE_ONLY') NOT NULL;

