package com.app.backend.repositories;

import com.app.backend.models.NguoiDung;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

public interface INguoiDungRepository extends JpaRepository<NguoiDung, Long> {
    boolean existsNguoiDungByTenDangNhap(String tenDangNhap);

    boolean existsNguoiDungByEmail(String email);

    Optional<NguoiDung> findNguoiDungByTenDangNhap(String tenDangNhap);

    Optional<NguoiDung> findByEmail(String email);

    @Query("SELECT o FROM NguoiDung o WHERE o.isXoa = false AND (:keyword IS NULL OR :keyword = '' OR " +
            "o.hoTen LIKE %:keyword% " +
            "OR o.diaChi LIKE %:keyword% " +
            "OR o.tenDangNhap LIKE %:keyword%) " +
            "AND LOWER(o.vaiTro.tenVaiTro) = 'user'")
    Page<NguoiDung> findAll(@Param("keyword") String keyword, Pageable pageable);

    List<NguoiDung> findByVaiTroId(Long roleId);

    @Query("SELECT o.vaiTro.id FROM NguoiDung o WHERE o.tenDangNhap = :tenDangNhap")
    Long findIdVaiTroByTenDangNhap(String tenDangNhap);

    long countByIsXoaFalse();

    @Query(value = """
            SELECT DATE(tao_luc) AS ngay, COUNT(*) AS so_luong
            FROM nguoi_dung
            WHERE tao_luc >= :from_time
            GROUP BY DATE(tao_luc)
            ORDER BY DATE(tao_luc)
            """, nativeQuery = true)
    List<Object[]> countNewUsersPerDayNative(@Param("from_time") Instant fromTime);

    // ================== ADMIN STATS ==================
    /** Tổng user */
    long count();
    
    /** Tổng user đang active (chưa xóa, chưa block) */
    @Query("SELECT COUNT(u) FROM NguoiDung u WHERE u.isXoa = false AND u.active = true")
    long countActive();
    
    /** Tổng user bị block */
    @Query("SELECT COUNT(u) FROM NguoiDung u WHERE u.isXoa = false AND u.active = false")
    long countBlocked();
    
    /** Tổng user đã xóa mềm */
    @Query("SELECT COUNT(u) FROM NguoiDung u WHERE u.isXoa = true")
    long countDeleted();
    
    /** Tổng admin */
    @Query("SELECT COUNT(u) FROM NguoiDung u WHERE u.isXoa = false AND LOWER(u.vaiTro.tenVaiTro) = 'admin'")
    long countAdmins();
    
    /** User đăng ký hôm nay */
    @Query("SELECT COUNT(u) FROM NguoiDung u WHERE u.taoLuc >= :startOfDay")
    long countRegisteredToday(@Param("startOfDay") Instant startOfDay);
    
    /** 
     * Lấy tất cả user cho admin (bao gồm cả đã xóa, bị block)
     */
    @Query("""
            SELECT u FROM NguoiDung u 
            WHERE (:keyword IS NULL OR :keyword = '' 
                   OR u.hoTen LIKE %:keyword% 
                   OR u.email LIKE %:keyword% 
                   OR u.tenDangNhap LIKE %:keyword%)
            ORDER BY u.id ASC
            """)
    Page<NguoiDung> findAllForAdmin(@Param("keyword") String keyword, Pageable pageable);
    
    /**
     * Export danh sách user (không phân trang)
     */
    @Query("""
            SELECT u FROM NguoiDung u 
            WHERE (:keyword IS NULL OR :keyword = '' 
                   OR u.hoTen LIKE %:keyword% 
                   OR u.email LIKE %:keyword% 
                   OR u.tenDangNhap LIKE %:keyword%)
            ORDER BY u.id ASC
            """)
    List<NguoiDung> findAllForExport(@Param("keyword") String keyword);

}
