-- Migration: Thêm CASUAL_ONLY vào enum loai_su_dung
-- Date: 2024
-- Description: Thêm giá trị CASUAL_ONLY vào enum loai_su_dung để tách biệt casual battle sets khỏi practice và course sets

-- Bước 1: Cập nhật enum loai_su_dung để thêm CASUAL_ONLY
ALTER TABLE bo_cau_hoi 
MODIFY COLUMN loai_su_dung ENUM('PRACTICE_ONLY', 'RANKED_ONLY', 'CASUAL_ONLY', 'BOTH') 
DEFAULT 'BOTH';

-- Bước 2: (Tùy chọn) Cập nhật các bộ câu hỏi hiện tại
-- Nếu muốn giữ nguyên logic cũ, các bộ câu hỏi có loai_su_dung = 'BOTH' vẫn sẽ hiển thị trong casual
-- Nếu muốn tách biệt rõ ràng, có thể chạy query sau để đánh dấu một số bộ là CASUAL_ONLY:
-- UPDATE bo_cau_hoi 
-- SET loai_su_dung = 'CASUAL_ONLY' 
-- WHERE loai_su_dung = 'BOTH' 
--   AND is_chinh_thuc = 0 
--   AND NOT EXISTS (
--     SELECT 1 FROM khoa_hoc_bo_cau_hoi khbch WHERE khbch.bo_cau_hoi_id = bo_cau_hoi.id
--   )
--   AND trang_thai = 'DA_DUYET';

