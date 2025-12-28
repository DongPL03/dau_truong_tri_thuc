-- =====================================================
-- PHASE 3: COMMUNITY SOCIAL - MIGRATION SQL
-- Version 3.1: Community Core Tables
-- =====================================================

-- =====================================================
-- Task 3.1.1: TAGS TABLE
-- =====================================================

-- Bảng Tags (Chủ đề/Thẻ phân loại bài viết)
CREATE TABLE IF NOT EXISTS tags (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    ten VARCHAR(100) NOT NULL COMMENT 'Tên tag hiển thị',
    slug VARCHAR(100) NOT NULL UNIQUE COMMENT 'Slug URL-friendly',
    mo_ta VARCHAR(500) COMMENT 'Mô tả ngắn về tag',
    mau_sac VARCHAR(7) DEFAULT '#6366F1' COMMENT 'Mã màu hex (vd: #FF5733)',
    icon VARCHAR(50) COMMENT 'Icon name hoặc emoji',
    so_bai_viet INT DEFAULT 0 COMMENT 'Số lượng bài viết có tag này',
    thu_tu INT DEFAULT 0 COMMENT 'Thứ tự hiển thị',
    hien_thi BOOLEAN DEFAULT TRUE COMMENT 'Tag có hiển thị không',
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    cap_nhat_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_tags_slug (slug),
    INDEX idx_tags_hien_thi (hien_thi),
    INDEX idx_tags_so_bai_viet (so_bai_viet DESC),
    INDEX idx_tags_thu_tu (thu_tu)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Task 3.1.2: BAI_VIET TABLE (Posts)
-- =====================================================

CREATE TABLE IF NOT EXISTS bai_viet (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    nguoi_dang_id BIGINT NOT NULL COMMENT 'FK -> nguoi_dung',
    tieu_de VARCHAR(255) NOT NULL COMMENT 'Tiêu đề bài viết',
    noi_dung TEXT COMMENT 'Nội dung bài viết (HTML from WYSIWYG)',
    loai_bai VARCHAR(20) DEFAULT 'THAO_LUAN' COMMENT 'THAO_LUAN, HOI_DAP, CHIA_SE, THONG_BAO',
    trang_thai VARCHAR(20) DEFAULT 'PENDING' COMMENT 'PENDING, APPROVED, HIDDEN, REJECTED, DELETED',
    luot_xem INT DEFAULT 0,
    luot_thich INT DEFAULT 0,
    luot_binh_luan INT DEFAULT 0,
    ghim BOOLEAN DEFAULT FALSE COMMENT 'Bài được ghim lên đầu',
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    cap_nhat_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    duyet_luc TIMESTAMP NULL COMMENT 'Thời gian duyệt bài',
    duyet_boi_id BIGINT NULL COMMENT 'Admin duyệt bài',
    
    INDEX idx_bai_viet_nguoi_dang (nguoi_dang_id),
    INDEX idx_bai_viet_trang_thai (trang_thai),
    INDEX idx_bai_viet_loai (loai_bai),
    INDEX idx_bai_viet_ghim (ghim),
    INDEX idx_bai_viet_tao_luc (tao_luc DESC),
    INDEX idx_bai_viet_hot (luot_thich DESC, luot_binh_luan DESC),
    
    CONSTRAINT fk_bai_viet_nguoi_dang FOREIGN KEY (nguoi_dang_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE,
    CONSTRAINT fk_bai_viet_duyet_boi FOREIGN KEY (duyet_boi_id) REFERENCES nguoi_dung(id) ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Bảng trung gian: Bài viết - Tags (Many-to-Many)
CREATE TABLE IF NOT EXISTS bai_viet_tag (
    bai_viet_id BIGINT NOT NULL,
    tag_id BIGINT NOT NULL,
    
    PRIMARY KEY (bai_viet_id, tag_id),
    CONSTRAINT fk_bvt_bai_viet FOREIGN KEY (bai_viet_id) REFERENCES bai_viet(id) ON DELETE CASCADE,
    CONSTRAINT fk_bvt_tag FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Task 3.1.3: BINH_LUAN TABLE (Comments - 2 level)
-- =====================================================

CREATE TABLE IF NOT EXISTS binh_luan (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    bai_viet_id BIGINT NOT NULL COMMENT 'FK -> bai_viet',
    nguoi_binh_luan_id BIGINT NOT NULL COMMENT 'FK -> nguoi_dung',
    binh_luan_cha_id BIGINT NULL COMMENT 'FK -> binh_luan (for replies)',
    noi_dung TEXT NOT NULL,
    luot_thich INT DEFAULT 0,
    bi_an BOOLEAN DEFAULT FALSE COMMENT 'Bị ẩn do vi phạm',
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    cap_nhat_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    INDEX idx_binh_luan_bai_viet (bai_viet_id),
    INDEX idx_binh_luan_nguoi (nguoi_binh_luan_id),
    INDEX idx_binh_luan_cha (binh_luan_cha_id),
    INDEX idx_binh_luan_tao_luc (tao_luc DESC),
    
    CONSTRAINT fk_binh_luan_bai_viet FOREIGN KEY (bai_viet_id) REFERENCES bai_viet(id) ON DELETE CASCADE,
    CONSTRAINT fk_binh_luan_nguoi FOREIGN KEY (nguoi_binh_luan_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE,
    CONSTRAINT fk_binh_luan_cha FOREIGN KEY (binh_luan_cha_id) REFERENCES binh_luan(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Task 3.1.4: LUOT_THICH TABLE (Likes for Posts)
-- =====================================================

CREATE TABLE IF NOT EXISTS luot_thich (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    bai_viet_id BIGINT NOT NULL,
    nguoi_dung_id BIGINT NOT NULL,
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE KEY uk_luot_thich (bai_viet_id, nguoi_dung_id),
    INDEX idx_luot_thich_nguoi (nguoi_dung_id),
    
    CONSTRAINT fk_luot_thich_bai_viet FOREIGN KEY (bai_viet_id) REFERENCES bai_viet(id) ON DELETE CASCADE,
    CONSTRAINT fk_luot_thich_nguoi FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Likes for Comments
CREATE TABLE IF NOT EXISTS luot_thich_binh_luan (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    binh_luan_id BIGINT NOT NULL,
    nguoi_dung_id BIGINT NOT NULL,
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE KEY uk_luot_thich_bl (binh_luan_id, nguoi_dung_id),
    INDEX idx_luot_thich_bl_nguoi (nguoi_dung_id),
    
    CONSTRAINT fk_luot_thich_bl_binh_luan FOREIGN KEY (binh_luan_id) REFERENCES binh_luan(id) ON DELETE CASCADE,
    CONSTRAINT fk_luot_thich_bl_nguoi FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Task 3.1.5: HINH_ANH_BAI_VIET TABLE (Post Images)
-- =====================================================

CREATE TABLE IF NOT EXISTS hinh_anh_bai_viet (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    bai_viet_id BIGINT NOT NULL,
    duong_dan VARCHAR(500) NOT NULL COMMENT 'Path to image file in uploads/images',
    thu_tu INT DEFAULT 0 COMMENT 'Thứ tự hiển thị',
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    INDEX idx_hinh_anh_bai_viet (bai_viet_id),
    
    CONSTRAINT fk_hinh_anh_bai_viet FOREIGN KEY (bai_viet_id) REFERENCES bai_viet(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Task 3.1.6: BAI_VIET_LUU TABLE (Saved Posts)
-- =====================================================

CREATE TABLE IF NOT EXISTS bai_viet_luu (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    bai_viet_id BIGINT NOT NULL,
    nguoi_dung_id BIGINT NOT NULL,
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE KEY uk_bai_viet_luu (bai_viet_id, nguoi_dung_id),
    INDEX idx_bai_viet_luu_nguoi (nguoi_dung_id),
    
    CONSTRAINT fk_bai_viet_luu_bai_viet FOREIGN KEY (bai_viet_id) REFERENCES bai_viet(id) ON DELETE CASCADE,
    CONSTRAINT fk_bai_viet_luu_nguoi FOREIGN KEY (nguoi_dung_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Task 3.1.7: BAO_CAO TABLE (Reports)
-- =====================================================

CREATE TABLE IF NOT EXISTS bao_cao (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    bai_viet_id BIGINT NULL COMMENT 'Báo cáo bài viết',
    binh_luan_id BIGINT NULL COMMENT 'Báo cáo bình luận',
    nguoi_bao_cao_id BIGINT NOT NULL,
    loai_bao_cao VARCHAR(20) NOT NULL COMMENT 'SPAM, NSFW, HARASSMENT, MISINFORMATION, COPYRIGHT, OTHER',
    chi_tiet VARCHAR(1000) COMMENT 'Mô tả chi tiết',
    trang_thai VARCHAR(20) DEFAULT 'PENDING' COMMENT 'PENDING, RESOLVED, DISMISSED',
    xu_ly_boi_id BIGINT NULL COMMENT 'Admin xử lý',
    ghi_chu_xu_ly VARCHAR(500),
    tao_luc TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    xu_ly_luc TIMESTAMP NULL,
    
    INDEX idx_bao_cao_bai_viet (bai_viet_id),
    INDEX idx_bao_cao_binh_luan (binh_luan_id),
    INDEX idx_bao_cao_trang_thai (trang_thai),
    INDEX idx_bao_cao_tao_luc (tao_luc DESC),
    
    CONSTRAINT fk_bao_cao_bai_viet FOREIGN KEY (bai_viet_id) REFERENCES bai_viet(id) ON DELETE CASCADE,
    CONSTRAINT fk_bao_cao_binh_luan FOREIGN KEY (binh_luan_id) REFERENCES binh_luan(id) ON DELETE CASCADE,
    CONSTRAINT fk_bao_cao_nguoi FOREIGN KEY (nguoi_bao_cao_id) REFERENCES nguoi_dung(id) ON DELETE CASCADE,
    CONSTRAINT fk_bao_cao_xu_ly FOREIGN KEY (xu_ly_boi_id) REFERENCES nguoi_dung(id) ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- DỮ LIỆU MẪU CHO TAGS
-- =====================================================

INSERT INTO tags (ten, slug, mo_ta, mau_sac, icon, thu_tu) VALUES
('Toán học', 'toan-hoc', 'Các câu hỏi và thảo luận về Toán học', '#3B82F6', '📐', 1),
('Vật lý', 'vat-ly', 'Các câu hỏi và thảo luận về Vật lý', '#EF4444', '⚡', 2),
('Hóa học', 'hoa-hoc', 'Các câu hỏi và thảo luận về Hóa học', '#10B981', '🧪', 3),
('Sinh học', 'sinh-hoc', 'Các câu hỏi và thảo luận về Sinh học', '#22C55E', '🧬', 4),
('Lịch sử', 'lich-su', 'Các câu hỏi và thảo luận về Lịch sử', '#F59E0B', '📜', 5),
('Địa lý', 'dia-ly', 'Các câu hỏi và thảo luận về Địa lý', '#06B6D4', '🌍', 6),
('Văn học', 'van-hoc', 'Các câu hỏi và thảo luận về Văn học', '#8B5CF6', '📚', 7),
('Tiếng Anh', 'tieng-anh', 'Các câu hỏi và thảo luận về Tiếng Anh', '#EC4899', '🔤', 8),
('Tin học', 'tin-hoc', 'Các câu hỏi và thảo luận về Tin học', '#6366F1', '💻', 9),
('Tips & Tricks', 'tips-tricks', 'Mẹo học tập và thi cử', '#F97316', '💡', 10),
('Hỏi đáp', 'hoi-dap', 'Hỏi đáp chung', '#14B8A6', '❓', 11),
('Chia sẻ kinh nghiệm', 'chia-se-kinh-nghiem', 'Chia sẻ kinh nghiệm học tập', '#84CC16', '🎯', 12),
('Thông báo', 'thong-bao', 'Thông báo từ hệ thống', '#EF4444', '📢', 13);
