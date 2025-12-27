package com.app.backend.services.thanhtich;

import com.app.backend.models.BangXepHang;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.NguoiDungThanhTich;
import com.app.backend.models.constant.AchievementCode;
import com.app.backend.models.constant.RankTier;
import com.app.backend.repositories.IBangXepHangRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.INguoiDungThanhTichRepository;
import com.app.backend.repositories.ITienDoKhoaHocRepository;
import com.app.backend.responses.achievement.AchievementResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ThanhTichService implements IThanhTichService {

    private final INguoiDungThanhTichRepository nguoiDungThanhTichRepository;
    private final IBangXepHangRepository bangXepHangRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final ITienDoKhoaHocRepository tienDoKhoaHocRepository;

    @Override
    @Transactional(readOnly = true)
    public List<AchievementResponse> getAchievementsOfUser(Long userId) {
        List<NguoiDungThanhTich> list =
                nguoiDungThanhTichRepository.findAllByNguoiDung_IdOrderByMoKhoaLucDesc(userId);
        return list.stream().map(AchievementResponse::from).toList();
    }

    @Override
    @Transactional
    public List<AchievementResponse> processAfterBattle(Long userId) {
        Optional<BangXepHang> opt = bangXepHangRepository.findByNguoiDung_Id(userId);
        if (opt.isEmpty()) {
            return List.of();
        }
        BangXepHang bxh = opt.get();

        int wins = Optional.ofNullable(bxh.getSoTranThang()).orElse(0);
        int matches = Optional.ofNullable(bxh.getTongTran()).orElse(0);
        RankTier tier = Optional.ofNullable(bxh.getRankTier()).orElse(RankTier.BRONZE);
        int level = Optional.ofNullable(bxh.getLevel()).orElse(1);
        long gold = Optional.ofNullable(bxh.getTienVang()).orElse(0L);


        List<AchievementResponse> unlocked = new ArrayList<>();

        // helper
        NguoiDung userRef = nguoiDungRepository.getReferenceById(userId);

        // Trận đấu
        unlockIfNeeded(userRef, AchievementCode.FIRST_MATCH, matches >= 1, unlocked);
        unlockIfNeeded(userRef, AchievementCode.TEN_MATCHES, matches >= 10, unlocked);
        unlockIfNeeded(userRef, AchievementCode.FIFTY_MATCHES, matches >= 50, unlocked);
        unlockIfNeeded(userRef, AchievementCode.HUNDRED_MATCHES, matches >= 100, unlocked);

        // Thắng
        unlockIfNeeded(userRef, AchievementCode.FIRST_WIN, wins >= 1, unlocked);
        unlockIfNeeded(userRef, AchievementCode.TEN_WINS, wins >= 10, unlocked);
        unlockIfNeeded(userRef, AchievementCode.FIFTY_WINS, wins >= 50, unlocked);

        // Level
        unlockIfNeeded(userRef, AchievementCode.LEVEL_5, level >= 5, unlocked);
        unlockIfNeeded(userRef, AchievementCode.LEVEL_10, level >= 10, unlocked);
        unlockIfNeeded(userRef, AchievementCode.LEVEL_20, level >= 20, unlocked);
        unlockIfNeeded(userRef, AchievementCode.LEVEL_30, level >= 30, unlocked);
        unlockIfNeeded(userRef, AchievementCode.LEVEL_40, level >= 40, unlocked);
        unlockIfNeeded(userRef, AchievementCode.LEVEL_50, level >= 50, unlocked);

        // Gold
        unlockIfNeeded(userRef, AchievementCode.GOLD_350, gold >= 350, unlocked);
        unlockIfNeeded(userRef, AchievementCode.GOLD_400, gold >= 400, unlocked);
        unlockIfNeeded(userRef, AchievementCode.GOLD_500, gold >= 500, unlocked);
        unlockIfNeeded(userRef, AchievementCode.GOLD_1000, gold >= 1000, unlocked);
        unlockIfNeeded(userRef, AchievementCode.GOLD_2000, gold >= 2000, unlocked);

        // Rank tier
        unlockIfNeeded(userRef, AchievementCode.REACH_SILVER, tier.compareTo(RankTier.SILVER) >= 0, unlocked);
        unlockIfNeeded(userRef, AchievementCode.REACH_GOLD, tier.compareTo(RankTier.GOLD) >= 0, unlocked);
        unlockIfNeeded(userRef, AchievementCode.REACH_PLATINUM, tier.compareTo(RankTier.PLATINUM) >= 0, unlocked);
        unlockIfNeeded(userRef, AchievementCode.REACH_DIAMOND, tier.compareTo(RankTier.DIAMOND) >= 0, unlocked);
        unlockIfNeeded(userRef, AchievementCode.REACH_MASTER, tier == RankTier.MASTER, unlocked);

        return unlocked;
    }

    @Override
    @Transactional
    public List<AchievementResponse> processAfterCourseComplete(Long userId) {
        // Đếm số khóa học đã hoàn thành
        long soKhoaHocHoanThanh = tienDoKhoaHocRepository.findByNguoiDungIdOrderByCapNhatLucDesc(userId)
                .stream()
                .filter(td -> "HOAN_THANH".equals(td.getTrangThai()))
                .count();

        List<AchievementResponse> unlocked = new ArrayList<>();
        NguoiDung userRef = nguoiDungRepository.getReferenceById(userId);

        // Unlock achievement dựa trên số khóa học đã hoàn thành
        unlockIfNeeded(userRef, AchievementCode.FIRST_COURSE_COMPLETE, soKhoaHocHoanThanh >= 1, unlocked);
        unlockIfNeeded(userRef, AchievementCode.FIVE_COURSES_COMPLETE, soKhoaHocHoanThanh >= 5, unlocked);
        unlockIfNeeded(userRef, AchievementCode.TEN_COURSES_COMPLETE, soKhoaHocHoanThanh >= 10, unlocked);
        unlockIfNeeded(userRef, AchievementCode.TWENTY_COURSES_COMPLETE, soKhoaHocHoanThanh >= 20, unlocked);

        // Kiểm tra điểm cao trong khóa học vừa hoàn thành
        // (Logic này sẽ được gọi từ TienDoKhoaHoiService với thông tin cụ thể về khóa học)
        
        return unlocked;
    }

    private void unlockIfNeeded(
            NguoiDung user,
            AchievementCode code,
            boolean condition,
            List<AchievementResponse> out
    ) {
        if (!condition) return;

        Long userId = user.getId();
        boolean existed = nguoiDungThanhTichRepository
                .existsByNguoiDung_IdAndCode(userId, code);
        if (existed) return;

        NguoiDungThanhTich entity = NguoiDungThanhTich.builder()
                .nguoiDung(user)
                .code(code)
                .moTa(code.getDescription())
                .moKhoaLuc(Instant.now())
                .build();

        NguoiDungThanhTich saved = nguoiDungThanhTichRepository.save(entity);
        out.add(AchievementResponse.from(saved));
    }

    @Override
    @Transactional
    public AchievementResponse unlockAchievement(Long userId, AchievementCode achievementCode) {
        NguoiDung user = nguoiDungRepository.getReferenceById(userId);
        
        // Kiểm tra đã có achievement này chưa
        boolean existed = nguoiDungThanhTichRepository
                .existsByNguoiDung_IdAndCode(userId, achievementCode);
        if (existed) {
            // Nếu đã có thì trả về achievement hiện tại
            return nguoiDungThanhTichRepository
                    .findByNguoiDung_IdAndCode(userId, achievementCode)
                    .map(AchievementResponse::from)
                    .orElse(null);
        }

        NguoiDungThanhTich entity = NguoiDungThanhTich.builder()
                .nguoiDung(user)
                .code(achievementCode)
                .moTa(achievementCode.getDescription())
                .moKhoaLuc(Instant.now())
                .build();

        NguoiDungThanhTich saved = nguoiDungThanhTichRepository.save(entity);
        return AchievementResponse.from(saved);
    }
}