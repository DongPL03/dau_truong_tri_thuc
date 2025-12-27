package com.app.backend.repositories;

import com.app.backend.models.TranDau;
import com.app.backend.models.constant.TrangThaiTranDau;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;

public interface ITranDauRepository extends JpaRepository<TranDau, Long> {
    Page<TranDau> findByTrangThai(String trangThai, Pageable pageable);
    Page<TranDau> findByTrangThaiAndLoaiTranDau(
            String trangThai,
            String loaiTranDau,
            Pageable pageable
    );


    @Override
    long count();

    long countByTrangThai(String trangThai);
    
    /**
     * Đếm số trận được tạo từ thời điểm nhất định (hôm nay)
     */
    @Query("SELECT COUNT(td) FROM TranDau td WHERE td.taoLuc >= :from")
    long countCreatedToday(@Param("from") Instant from);

    /**
     * Đếm số trận theo ngày (dùng cột taoLuc / created time của trận)
     */
    @Query("""
            select function('date', td.taoLuc) as ngay, count(td)
            from TranDau td
            where td.taoLuc >= :from
            group by function('date', td.taoLuc)
            order by ngay asc
            """)
    List<Object[]> countBattlesByDaySince(@Param("from") Instant from);

    /**
     * Top bộ câu hỏi được dùng nhiều nhất (tính theo số trận đã FINISHED)
     */
    @Query("""
            select b.id, b.tieuDe, count(td)
            from TranDau td
                join td.boCauHoi b
            where td.trangThai = :trangThai
            group by b.id, b.tieuDe
            order by count(td) desc
            """)
    Page<Object[]> findTopBoCauHoiByBattleCount(
            @Param("trangThai") String trangThai,
            Pageable pageable
    );
}
