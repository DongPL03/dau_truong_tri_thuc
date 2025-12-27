-- Migration: Thêm cột muon_tao_tra_phi vào bảng bo_cau_hoi
-- Date: 2024
-- Description: Thêm trường để user chỉ định muốn tạo bộ câu hỏi trả phí hay miễn phí

-- Thêm cột muon_tao_tra_phi
ALTER TABLE bo_cau_hoi 
ADD COLUMN muon_tao_tra_phi TINYINT(1) DEFAULT 0 
COMMENT 'User muốn tạo bộ câu hỏi trả phí hay không (0 = miễn phí, 1 = trả phí)';

-- Cập nhật các bộ câu hỏi hiện tại: mặc định là miễn phí (0)
UPDATE bo_cau_hoi 
SET muon_tao_tra_phi = 0 
WHERE muon_tao_tra_phi IS NULL;

