-- Migration: Thêm COURSE_ONLY và COURSE_AND_PRACTICE vào enum loai_su_dung
-- Date: 2024

-- Bước 1: Thay đổi kiểu cột từ ENUM sang VARCHAR để có thể thêm giá trị mới
ALTER TABLE bo_cau_hoi 
MODIFY COLUMN loai_su_dung VARCHAR(50);

-- Bước 2: Cập nhật lại thành ENUM với các giá trị mới
ALTER TABLE bo_cau_hoi 
MODIFY COLUMN loai_su_dung ENUM('PRACTICE_ONLY','RANKED_ONLY','CASUAL_ONLY','COURSE_ONLY','COURSE_AND_PRACTICE');

-- Kiểm tra: Xem các giá trị hiện tại
-- SELECT DISTINCT loai_su_dung FROM bo_cau_hoi;

