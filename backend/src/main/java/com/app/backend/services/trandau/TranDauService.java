package com.app.backend.services.trandau;

import com.app.backend.components.BattleLoopTask;
import com.app.backend.components.BattleStateManager;
import com.app.backend.components.BattleWsPublisher;
import com.app.backend.dtos.*;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.*;
import com.app.backend.models.constant.LuatTinhDiem;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.*;
import com.app.backend.responses.lichsutrandau.LichSuTranDauResponse;
import com.app.backend.responses.trandau.*;
import com.app.backend.responses.websocket.FinishedEvent;
import com.app.backend.responses.websocket.LeaderboardUpdateEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TranDauService implements ITranDauService {

    private final ITranDauRepository tranDauRepository;
    private final INguoiChoiTranDauRepository nguoiChoiTranDauRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ICauHoiRepository cauHoiRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final BattleStateManager battleStateManager;
    private final BattleLoopTask battleLoopTask;
    private final BattleWsPublisher wsPublisher;
    private final ITraLoiTranDauRepository traLoiTranDauRepository;
    private final ILichSuTranDauRepository lichSuTranDauRepository;

    private String generateRoomCode(int length) {
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        Random random = new Random();
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++)
            sb.append(chars.charAt(random.nextInt(chars.length())));
        return sb.toString();
    }

    @Transactional
    @Override
    public TranDau taoPhong(TaoTranDauDTO taoTranDauDTO, Long currentUserId) throws Exception {
        NguoiDung host = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        BoCauHoi bo = boCauHoiRepository.findById(taoTranDauDTO.getBoCauHoiId())
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        if (Boolean.FALSE.equals(taoTranDauDTO.getCongKhai()) &&
                (taoTranDauDTO.getMaPin() == null || taoTranDauDTO.getMaPin().isBlank())) {
            throw new IllegalArgumentException("Phòng riêng tư bắt buộc có mã PIN");
        }
        if (taoTranDauDTO.getGioiHanNguoiChoi() < 2 || taoTranDauDTO.getGioiHanNguoiChoi() > 4) {
            throw new IllegalArgumentException("Giới hạn người chơi phải từ 2 – 4");
        }
        TranDau tranDau = new TranDau();
        tranDau.setTenPhong(taoTranDauDTO.getTenPhong());
        tranDau.setBoCauHoi(bo);
        tranDau.setChuPhong(host);
        tranDau.setCongKhai(taoTranDauDTO.getCongKhai());
        tranDau.setMaPin(taoTranDauDTO.getCongKhai() ? null : taoTranDauDTO.getMaPin());
        tranDau.setMaPhong(generateRoomCode(6));
        tranDau.setGioiHanNguoiChoi(taoTranDauDTO.getGioiHanNguoiChoi());
        tranDau.setGioiHanThoiGianCauGiay(taoTranDauDTO.getGioiHanThoiGianCauGiay());
        // Luật tính điểm nếu có enum:
        tranDau.setLuatTinhDiem(taoTranDauDTO.getLuatTinhDiem());
        tranDau.setTrangThai(TrangThaiTranDau.PENDING);

        TranDau saved = tranDauRepository.save(tranDau);

        // tự động cho host vào phòng
        nguoiChoiTranDauRepository.save(
                NguoiChoiTranDau.builder().tranDau(saved).nguoiDung(host).build()
        );

        return saved;
    }

    @Transactional
    @Override
    public TranDau thamGia(ThamGiaTranDauDTO thamGiaTranDauDTO, Long currentUserId) throws Exception {
        TranDau tranDau = tranDauRepository.findById(thamGiaTranDauDTO.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        if (!Objects.equals(tranDau.getTrangThai(), TrangThaiTranDau.PENDING)) {
            throw new IllegalStateException("Phòng không ở trạng thái chờ");
        }

        long current = nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());
        if (current >= tranDau.getGioiHanNguoiChoi()) {
            throw new IllegalStateException("Phòng đã đủ người");
        }

        // 🔒 Nếu phòng private → yêu cầu mã PIN
        if (Boolean.FALSE.equals(tranDau.getCongKhai())) {
            if (thamGiaTranDauDTO.getMaPin() == null || !thamGiaTranDauDTO.getMaPin().equals(tranDau.getMaPin())) {
                throw new SecurityException("Sai mã PIN hoặc phòng này riêng tư");
            }
        }

        // đã tham gia chưa?
        boolean existed = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDau.getId(), currentUserId)
                .isPresent();
        if (existed) return tranDau; // idempotent

        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        nguoiChoiTranDauRepository.save(
                NguoiChoiTranDau.builder().tranDau(tranDau).nguoiDung(user).build()
        );

        // Phát WS
        int soNguoi = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());
        if (soNguoi > tranDau.getGioiHanNguoiChoi()) {
            throw new IllegalStateException("Phòng đã đủ số lượng người chơi.");
        }
        wsPublisher.publishPlayerJoined(tranDau.getId(), user.getId(), user.getHoTen(), soNguoi);

        // Phát bảng xếp hạng rỗng
        updateAndBroadcastLeaderboard(tranDau.getId(), null);

        return tranDau;
    }

    @Transactional
    @Override
    public void roiPhong(RoiTranDauDTO dto, Long currentUserId) throws Exception {
        TranDau tranDau = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        if (tranDau.getTrangThai() == TrangThaiTranDau.ONGOING
                && tranDau.getChuPhong() != null
                && tranDau.getChuPhong().getId().equals(user.getId())) {
            throw new IllegalStateException("Chủ phòng không thể rời phòng khi trận đang diễn ra. Hãy kết thúc trận trước.");
        }

        NguoiChoiTranDau nctd = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDau.getId(), currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa ở trong phòng"));
        // Nếu host rời phòng khi đang PENDING → có thể chuyển host cho người khác hoặc xoá phòng.
        // Bước 1: đơn giản là xoá người chơi ra khỏi phòng.

        nguoiChoiTranDauRepository.delete(nctd);

        int soNguoi = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());
        wsPublisher.publishPlayerLeft(tranDau.getId(), nctd.getNguoiDung().getId(), nctd.getNguoiDung().getHoTen(), soNguoi);

//        // Nếu không còn ai trong phòng → xoá phòng
//        long remain = nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());

        if (Objects.equals(tranDau.getChuPhong().getId(), currentUserId) && soNguoi > 0) {
            nguoiChoiTranDauRepository.findFirstByTranDau_IdOrderByIdAsc(tranDau.getId())
                    .ifPresent(next -> tranDau.setChuPhong(next.getNguoiDung()));
        }

        // Nếu phòng trống và chưa bắt đầu → xoá

        if (soNguoi == 0 && TrangThaiTranDau.PENDING.equals(tranDau.getTrangThai()))
            tranDauRepository.delete(tranDau);

        updateAndBroadcastLeaderboard(tranDau.getId(), null);

    }

    @Transactional(readOnly = true)
    @Override
    public TranDau chiTietPhong(Long tranDauId) throws Exception {
        return tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
    }

    @Transactional(readOnly = true)
    @Override
    public TranDauResponse getBattleDetailResponse(Long tranDauId) throws Exception {
        // 1. Lấy thông tin trận đấu
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        // 2. 🔥 Tính số lượng người chơi hiện tại trong phòng
        int soLuong = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDauId);

        // 3. Map sang DTO và trả về (truyền số lượng vào)
        return TranDauResponse.fromEntity(td, soLuong);
    }

    @Transactional(readOnly = true)
    @Override
    public TranDauResponse getBattleDetailResponse(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        int soLuong = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDauId);

        // 👇 kiểm tra user hiện tại có đang ở bảng nguoi_choi_tran_dau không
        boolean daThamGia = nguoiChoiTranDauRepository
                .existsByTranDauIdAndNguoiDungId(tranDauId, currentUserId);

        TranDauResponse res = TranDauResponse.fromEntity(td, soLuong);
        res.setDaThamGia(daThamGia);
        return res;
    }


    @Transactional(readOnly = true)
    @Override
    public Page<TranDau> danhSachPhongCho(PageRequest pageRequest) {
        return tranDauRepository.findByTrangThai(TrangThaiTranDau.PENDING, pageRequest);
    }

    @Override
    @Transactional
    public BattleStartResponse startBattle(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        // 🔒 Chỉ chủ phòng mới được bắt đầu
        if (!td.getChuPhong().getId().equals(currentUserId)) {
            throw new SecurityException("Chỉ chủ phòng mới có quyền bắt đầu trận");
        }

        // ⛔ Không được start nếu đang không ở trạng thái chờ
        if (!TrangThaiTranDau.PENDING.equals(td.getTrangThai())) {
            throw new IllegalStateException("Phòng không ở trạng thái chờ");
        }

        // 📋 Lấy danh sách câu hỏi
        List<CauHoi> danhSachCauHoi = cauHoiRepository.findByBoCauHoiId(td.getBoCauHoi().getId());
        if (danhSachCauHoi.isEmpty()) {
            throw new IllegalStateException("Bộ câu hỏi này không có câu hỏi nào");
        }

        // 🔀 Trộn câu hỏi
        Collections.shuffle(danhSachCauHoi);

        // 🧭 Cập nhật DB
        td.setTrangThai(TrangThaiTranDau.ONGOING);
        td.setBatDauLuc(Instant.now());
        tranDauRepository.save(td);

        // 🧠 Khởi tạo BattleState mới trong RAM
        BattleState state = new BattleState();
        state.setTranDauId(td.getId());
        state.setDanhSachCauHoi(danhSachCauHoi);
        state.setStartTime(Instant.now());

        // ⏱ Thiết lập thời gian mỗi câu
        int seconds = (td.getGioiHanThoiGianCauGiay() != null)
                ? td.getGioiHanThoiGianCauGiay()
                : 15;
        state.setSecondsPerQuestion(seconds);

        // 🧍‍♂️ Khởi tạo điểm 0 cho toàn bộ người chơi
        Set<Long> playerIds = nguoiChoiTranDauRepository
                .findByTranDau_Id(td.getId(), null)
                .stream()
                .map(nctd -> nctd.getNguoiDung().getId())
                .collect(Collectors.toSet());
        // cũng thêm cả host vào
        playerIds.add(td.getChuPhong().getId());
        state.initScoresForPlayers(playerIds);

        // Lưu state vào manager
        battleStateManager.save(state);

        wsPublisher.publishBattleStarted(td.getId(), td.getTenPhong(), td.getBatDauLuc(), danhSachCauHoi.size(), seconds, 10);
        battleLoopTask.runAutoLoop(td.getId(), seconds);

        // ✅ Trả kết quả khởi tạo
        return BattleStartResponse.from(td, danhSachCauHoi);
    }

    @Override
    @Transactional
    public SubmitAnswerResponse submitAnswer(SubmitAnswerDTO dto, Long currentUserId) throws Exception {
        // 1️⃣ Kiểm tra trận đấu hợp lệ
        TranDau td = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        if (!TrangThaiTranDau.ONGOING.equals(td.getTrangThai())) {
            throw new IllegalStateException("Phòng không ở trạng thái đang diễn ra");
        }

        // 2️⃣ Xác nhận người chơi có trong phòng
        boolean inRoom = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(td.getId(), currentUserId)
                .isPresent();
        if (!inRoom && !Objects.equals(td.getChuPhong().getId(), currentUserId)) {
            throw new SecurityException("Bạn không ở trong phòng này");
        }

        // 3️⃣ Lấy BattleState hiện tại
        BattleState state = battleStateManager.get(td.getId());
        if (state == null || state.getCurrentQuestionIndex() < 0) {
            throw new IllegalStateException("Chưa có câu hỏi nào đang bật");
        }

        int idx = state.getCurrentQuestionIndex();
        CauHoi q = state.getDanhSachCauHoi().get(idx);

        if (!Objects.equals(q.getId(), dto.getCauHoiId())) {
            throw new IllegalArgumentException("Câu hỏi không khớp với câu hiện tại");
        }

        // 4️⃣ Ghi nhận đáp án (atomic)
        String ans = dto.getAnswer().trim().toUpperCase();
        boolean firstSubmit = state.recordAnswer(idx, currentUserId, ans);
        if (!firstSubmit) {
            throw new IllegalStateException("Bạn đã nộp đáp án cho câu này rồi");
        }

        // 5️⃣ Kiểm tra timeout
        int seconds = state.getSecondsPerQuestion();
        long totalMs = seconds * 1000L;
        long elapsedMs = Duration.between(state.getCurrentQuestionStart(), Instant.now()).toMillis();
        boolean withinTime = elapsedMs <= totalMs;

        // 6️⃣ Tính điểm
        boolean correct = withinTime && ans.equalsIgnoreCase(String.valueOf(q.getDapAnDung()));
        int gained = 0;
        if (correct) {
            if (LuatTinhDiem.SPEED_BONUS.equalsIgnoreCase(td.getLuatTinhDiem())) {
                long remain = Math.max(0, totalMs - elapsedMs);
                double ratio = (double) remain / (double) totalMs;
                gained = (int) Math.max(100, Math.round(1000 * ratio));
            } else {
                gained = 100;
            }
        }

        // 7️⃣ Cập nhật điểm tổng
        int total = state.addScore(currentUserId, gained);
        battleStateManager.save(state);

        // ⭐ 7.1) Cập nhật DB: diem & so_cau_dung của người nộp
        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        NguoiChoiTranDau player = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(td.getId(), user.getId())
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa tham gia trận"));

        int cur = Optional.ofNullable(player.getDiem()).orElse(0);
        player.setDiem(cur + gained);
        if (correct) {
            player.setSoCauDung(Optional.ofNullable(player.getSoCauDung()).orElse(0) + 1);
        }
        nguoiChoiTranDauRepository.save(player);


        // 8️⃣ Lưu log trả lời
//        NguoiDung user = nguoiDungRepository.findById(currentUserId)
//                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));
//        traLoiTranDauRepository.save(
//                TraLoiTranDau.builder()
//                        .tranDau(td)
//                        .nguoiDung(user)
//                        .cauHoi(q)
//                        .luaChon(ans.charAt(0))
//                        .dungHaySai(correct)
//                        .thoiGianMs((int) elapsedMs)
//                        .build()
//        );
        traLoiTranDauRepository.save(TraLoiTranDau.builder()
                .tranDau(td)
                .nguoiDung(user)
                .cauHoi(q)
                .luaChon(ans.charAt(0))
                .dungHaySai(correct)
                .thoiGianMs((int) elapsedMs)
                .build());

        // 9️⃣ Phát sự kiện WS cập nhật điểm cho người chơi này
        wsPublisher.publishScoreUpdate(
                td.getId(), user.getId(), user.getHoTen(), correct, gained, total, idx
        );

        // 🔟 Cập nhật và broadcast leaderboard tổng thể
//        updateAndBroadcastLeaderboard(td.getId(), state);
        updateAndBroadcastLeaderboard(td.getId());
        // 1️⃣1️⃣ Trả response cho client
        return SubmitAnswerResponse.builder()
                .correct(correct)
                .gainedPoints(gained)
                .totalPoints(total)
                .questionIndex(idx)
                .build();
    }


    //    @Override
//    @Transactional
//    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
//        TranDau td = tranDauRepository.findById(tranDauId)
//                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
//
//        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId)) {
//            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
//        }
//
//        // Nếu đã kết thúc rồi, trả kết quả cũ
//        if ("FINISHED".equals(td.getTrangThai())) {
//            return BattleFinishResponse.from(td, null, null);
//        }
//
//        // ✅ Lấy BattleState trong RAM
//        BattleState state = battleStateManager.get(td.getId());
//        Map<Long, Integer> scores = (state != null) ? state.getDiemNguoiChoi() : new HashMap<>();
//
//        // ✅ Tính người thắng
//        Long winnerId = null;
//        String winnerTen = null;
//        if (!scores.isEmpty()) {
//            // Lấy người có điểm cao nhất
//            var topEntry = scores.entrySet().stream()
//                    .max(Map.Entry.comparingByValue())
//                    .orElse(null);
//            if (topEntry != null) {
//                winnerId = topEntry.getKey();
//                NguoiDung winnerUser = nguoiDungRepository.findById(winnerId).orElse(null);
//                winnerTen = (winnerUser != null) ? winnerUser.getHoTen() : "Người chơi";
//                td.setWinner(winnerUser);
//            }
//        }
//
//        // ✅ Cập nhật DB
//        td.setTrangThai("FINISHED");
//        td.setKetThucLuc(LocalDateTime.now());
//        tranDauRepository.save(td);
//
//        // ✅ Lấy danh sách người chơi (để hiển thị tên)
//        List<NguoiDung> allUsers = nguoiDungRepository.findAllById(scores.keySet());
//        Map<Long, String> nameMap = allUsers.stream()
//                .collect(Collectors.toMap(NguoiDung::getId, NguoiDung::getHoTen));
//
//        // ✅ Tạo danh sách bảng điểm (sắp giảm dần theo điểm)
//        AtomicInteger rankCounter = new AtomicInteger(1);
//        List<BattleFinishResponse.PlayerScore> list = scores.entrySet().stream()
//                .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
//                .map(e -> BattleFinishResponse.PlayerScore.builder()
//                        .userId(e.getKey())
//                        .hoTen(nameMap.getOrDefault(e.getKey(), "Người chơi"))
//                        .diem(e.getValue())
//                        .thuHang(rankCounter.getAndIncrement())
//                        .build())
//                .toList();
//
//
//        // ✅ Dọn cache state
//        battleStateManager.remove(td.getId());
//        for (Map.Entry<Long, Integer> entry : scores.entrySet()) {
//            Long uid = entry.getKey();
//            int total = entry.getValue();
//
//            // Đếm số câu đúng trong state
//            int correctCount = (int) state.getAnswers().values().stream()
//                    .filter(map -> {
//                        String ans = map.get(uid);
//                        if (ans == null) return false;
//                        // Tìm câu hỏi tương ứng để so sánh đáp án
//                        int idx = state.getAnswers().values().stream().toList().indexOf(map);
//                        return ans.equalsIgnoreCase(String.valueOf(
//                                state.getDanhSachCauHoi().get(idx).getDapAnDung()));
//                    })
//                    .count();
//
//            // Cập nhật bảng người chơi trận đấu
//            nguoiChoiTranDauRepository.findByTranDau_IdAndNguoiDung_Id(td.getId(), uid)
//                    .ifPresent(nctd -> {
//                        nctd.setDiem(total);
//                        nctd.setSoCauDung(correctCount);
//                        nguoiChoiTranDauRepository.save(nctd);
//                    });
//
//            // Lưu lịch sử trận đấu
//            NguoiDung nd = nguoiDungRepository.findById(uid).orElse(null);
//            if (nd != null) {
//                LichSuTranDau lichSu = LichSuTranDau.builder()
//                        .tranDau(td)
//                        .nguoiDung(nd)
//                        .tongDiem(total)
//                        .tongCauDung(correctCount)
//                        .hoanThanhLuc(LocalDateTime.now())
//                        .build();
//                lichSuTranDauRepository.save(lichSu);
//            }
//        }
//
//
//        // ✅ --- PHÁT SỰ KIỆN WEBSOCKET ---
//        FinishedEvent.Winner win = (winnerId != null)
//                ? FinishedEvent.Winner.builder().userId(winnerId).hoTen(winnerTen).build()
//                : null;
//
//        List<FinishedEvent.Player> players = list.stream()
//                .map(p -> FinishedEvent.Player.builder()
//                        .userId(p.getUserId())
//                        .hoTen(p.getHoTen())
//                        .diem(p.getDiem())
//                        .thuHang(p.getThuHang())
//                        .build())
//                .toList();
//
//        wsPublisher.publishFinished(
//                td.getId(),
//                td.getTenPhong(),
//                td.getMaPhong(),
//                td.getBatDauLuc(),
//                td.getKetThucLuc(),
//                win,
//                players
//        );
//
//        // ✅ Trả response cuối cùng cho API
//        return BattleFinishResponse.from(td, scores, allUsers);
//    }
//    @Override
//    @Transactional
//    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
//        TranDau td = tranDauRepository.findById(tranDauId)
//                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
//        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId))
//            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận");
//
//        if ("FINISHED".equals(td.getTrangThai()))
//            return BattleFinishResponse.from(td, null, null);
//
//        BattleState state = battleStateManager.get(td.getId());
//        Map<Long, Integer> scores = (state != null) ? state.getDiemNguoiChoi() : new HashMap<>();
//
//        Long winnerId = null;
//        String winnerTen = null;
//        if (!scores.isEmpty()) {
//            var top = scores.entrySet().stream()
//                    .max(Map.Entry.comparingByValue()).orElse(null);
//            if (top != null) {
//                winnerId = top.getKey();
//                NguoiDung w = nguoiDungRepository.findById(winnerId).orElse(null);
//                winnerTen = (w != null) ? w.getHoTen() : "Người chơi";
//                td.setWinner(w);
//            }
//        }
//
//        td.setTrangThai("FINISHED");
//        td.setKetThucLuc(LocalDateTime.now());
//        tranDauRepository.save(td);
//
//        List<NguoiDung> allUsers = nguoiDungRepository.findAllById(scores.keySet());
//        Map<Long, String> nameMap = allUsers.stream()
//                .collect(Collectors.toMap(NguoiDung::getId, NguoiDung::getHoTen));
//
//        AtomicInteger rank = new AtomicInteger(1);
//        List<BattleFinishResponse.PlayerScore> list = scores.entrySet().stream()
//                .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
//                .map(e -> BattleFinishResponse.PlayerScore.builder()
//                        .userId(e.getKey())
//                        .hoTen(nameMap.getOrDefault(e.getKey(), "Người chơi"))
//                        .diem(e.getValue())
//                        .thuHang(rank.getAndIncrement()).build())
//                .toList();
//
//        // Lưu lịch sử / cập nhật người chơi
//        for (var e : scores.entrySet()) {
//            Long uid = e.getKey();
//            int diem = e.getValue();
//            int soCauDung = (int) state.getAnswers().values().stream()
//                    .filter(m -> m.containsKey(uid))
//                    .filter(m -> {
//                        String ans = m.get(uid);
//                        int idx = state.getAnswers().values().stream().toList().indexOf(m);
//                        return ans.equalsIgnoreCase(String.valueOf(state.getDanhSachCauHoi().get(idx).getDapAnDung()));
//                    }).count();
//
//            nguoiChoiTranDauRepository.findByTranDau_IdAndNguoiDung_Id(td.getId(), uid)
//                    .ifPresent(nctd -> {
//                        nctd.setDiem(diem);
//                        nctd.setSoCauDung(soCauDung);
//                        nguoiChoiTranDauRepository.save(nctd);
//                    });
//
//            nguoiDungRepository.findById(uid).ifPresent(nd -> {
//                LichSuTranDau lichSu = LichSuTranDau.builder()
//                        .tranDau(td).nguoiDung(nd)
//                        .tongDiem(diem).tongCauDung(soCauDung)
//                        .hoanThanhLuc(LocalDateTime.now()).build();
//                lichSuTranDauRepository.save(lichSu);
//            });
//        }
//
//        battleStateManager.remove(td.getId());
//
//        wsPublisher.publishFinished(
//                td.getId(), td.getTenPhong(), td.getMaPhong(),
//                td.getBatDauLuc(), td.getKetThucLuc(),
//                (winnerId != null) ? FinishedEvent.Winner.builder().userId(winnerId).hoTen(winnerTen).build() : null,
//                list.stream().map(p -> FinishedEvent.Player.builder()
//                        .userId(p.getUserId()).hoTen(p.getHoTen())
//                        .diem(p.getDiem()).thuHang(p.getThuHang()).build()).toList()
//        );
//
//        return BattleFinishResponse.from(td, scores, allUsers);
//    }
//    @Override
//    @Transactional
//    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
//        TranDau td = tranDauRepository.findById(tranDauId)
//                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
//
//        // 1️⃣ Nếu không phải auto → chỉ host mới có quyền kết thúc
//        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId)) {
//            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
//        }
//
//        // 2️⃣ Nếu đã kết thúc rồi → trả lại kết quả cũ
//        if (TrangThaiTranDau.FINISHED.equals(td.getTrangThai())) {
//            return BattleFinishResponse.from(td, null, null);
//        }
//
//        // 3️⃣ Lấy trạng thái đang lưu trong RAM
//        BattleState state = battleStateManager.get(tranDauId);
//        if (state != null && !state.markFinishedOnce()) {
//            // Đã finish trước đó
//            return BattleFinishResponse.from(td, state.getDiemNguoiChoi(), null);
//        }
//
//        Map<Long, Integer> scores = (state != null) ? new HashMap<>(state.getDiemNguoiChoi()) : new HashMap<>();
//
//        // Nếu không có điểm nào trong state → fallback từ DB (đảm bảo an toàn)
//        if (scores.isEmpty()) {
//            nguoiChoiTranDauRepository.findByTranDau_Id(td.getId(), null)
//                    .forEach(p -> scores.put(p.getNguoiDung().getId(), p.getDiem()));
//        }
//
//        // 4️⃣ Xác định người thắng
//        Long winnerId = null;
//        String winnerTen = null;
//        if (!scores.isEmpty()) {
//            var top = scores.entrySet().stream().max(Map.Entry.comparingByValue()).orElse(null);
//            if (top != null) {
//                winnerId = top.getKey();
//                NguoiDung w = nguoiDungRepository.findById(winnerId).orElse(null);
//                winnerTen = (w != null) ? w.getHoTen() : "Người chơi";
//                td.setWinner(w);
//            }
//        }
//
//        // 5️⃣ Cập nhật trạng thái & thời gian kết thúc
//        td.setTrangThai(TrangThaiTranDau.FINISHED);
//        td.setKetThucLuc(LocalDateTime.now());
//        tranDauRepository.save(td);
//
//        // 6️⃣ Lưu điểm + lịch sử cho từng người chơi
//        for (var entry : scores.entrySet()) {
//            Long uid = entry.getKey();
//            int diem = entry.getValue();
//
//            // Tính số câu đúng
//            int soCauDung = 0;
//            if (state != null) {
//                for (Map<Long, String> map : state.getAnswers().values()) {
//                    String ans = map.get(uid);
//                    if (ans == null) continue;
//                    int idx = new ArrayList<>(state.getAnswers().values()).indexOf(map);
//                    if (idx < 0 || idx >= state.getDanhSachCauHoi().size()) continue;
//                    CauHoi cau = state.getDanhSachCauHoi().get(idx);
//                    if (ans.equalsIgnoreCase(String.valueOf(cau.getDapAnDung()))) soCauDung++;
//                }
//            }
//            final int finalSoCauDung = soCauDung;
//            // Cập nhật DB cho người chơi trong phòng
//            nguoiChoiTranDauRepository.findByTranDau_IdAndNguoiDung_Id(td.getId(), uid)
//                    .ifPresent(nctd -> {
//                        nctd.setDiem(diem);
//                        nctd.setSoCauDung(finalSoCauDung);
//                        nguoiChoiTranDauRepository.save(nctd);
//                    });
//
//            // Lưu lịch sử
//
//            nguoiDungRepository.findById(uid).ifPresent(nd -> {
//                LichSuTranDau lichSu = LichSuTranDau.builder()
//                        .tranDau(td)
//                        .nguoiDung(nd)
//                        .tongDiem(diem)
//                        .tongCauDung(finalSoCauDung)
//                        .hoanThanhLuc(LocalDateTime.now())
//                        .build();
//                lichSuTranDauRepository.save(lichSu);
//            });
//        }
//
//        // 7️⃣ Chuẩn bị dữ liệu leaderboard để gửi WS
//        List<NguoiDung> allUsers = nguoiDungRepository.findAllById(scores.keySet());
//        Map<Long, String> nameMap = allUsers.stream()
//                .collect(Collectors.toMap(NguoiDung::getId, NguoiDung::getHoTen));
//
//        AtomicInteger rank = new AtomicInteger(1);
//        List<BattleFinishResponse.PlayerScore> list = scores.entrySet().stream()
//                .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
//                .map(e -> BattleFinishResponse.PlayerScore.builder()
//                        .userId(e.getKey())
//                        .hoTen(nameMap.getOrDefault(e.getKey(), "Người chơi"))
//                        .diem(e.getValue())
//                        .thuHang(rank.getAndIncrement())
//                        .build())
//                .toList();
//
//        // 8️⃣ Phát sự kiện FINISHED qua websocket
//        wsPublisher.publishFinished(
//                td.getId(),
//                td.getTenPhong(),
//                td.getMaPhong(),
//                td.getBatDauLuc(),
//                td.getKetThucLuc(),
//                (winnerId != null) ? FinishedEvent.Winner.builder()
//                        .userId(winnerId)
//                        .hoTen(winnerTen)
//                        .build() : null,
//                list.stream().map(p -> FinishedEvent.Player.builder()
//                                .userId(p.getUserId())
//                                .hoTen(p.getHoTen())
//                                .diem(p.getDiem())
//                                .thuHang(p.getThuHang())
//                                .build())
//                        .toList()
//        );
//
//        // 9️⃣ Dọn BattleState (chỉ khi autoMode hoặc sau khi publish xong)
//        battleStateManager.remove(td.getId());
//
//        // 🔟 Trả response
//        return BattleFinishResponse.from(td, scores, allUsers);
//    }
//    @Override
//    @Transactional
//    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
//        TranDau td = tranDauRepository.findById(tranDauId)
//                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
//
//        // 1️⃣ Quyền hạn
//        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId)) {
//            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
//        }
//
//        if (TrangThaiTranDau.FINISHED.equals(td.getTrangThai())) {
//            return BattleFinishResponse.from(td, null, null);
//        }
//
//        // 2️⃣ Lấy state
//        BattleState state = battleStateManager.get(tranDauId);
//        if (state != null && !state.markFinishedOnce()) {
//            return BattleFinishResponse.from(td, state.getDiemNguoiChoi(), null);
//        }
//
//        // 3️⃣ Lấy danh sách người chơi
//        List<NguoiChoiTranDau> players = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId());
//        Map<Long, Integer> scores = (state != null)
//                ? new HashMap<>(state.getDiemNguoiChoi())
//                : players.stream().collect(Collectors.toMap(p -> p.getNguoiDung().getId(), NguoiChoiTranDau::getDiem));
//
//        // 4️⃣ Tính số câu đúng (nếu có state)
//        Map<Long, Integer> correctMap = new HashMap<>();
//        if (state != null) {
//            for (NguoiChoiTranDau p : players) {
//                int uid = p.getNguoiDung().getId().intValue();
//                long correct = state.getDanhSachCauHoi().stream()
//                        .filter(c -> {
//                            Map<Long, String> answers = state.getAnswers().get(c.getId());
//                            return answers != null && answers.get((long) uid) != null &&
//                                    answers.get((long) uid).equalsIgnoreCase(String.valueOf(c.getDapAnDung()));
//                        })
//                        .count();
//                correctMap.put(p.getNguoiDung().getId(), (int) correct);
//            }
//        }
//
//        // 5️⃣ Cập nhật DB đồng loạt
//        players.forEach(p -> {
//            int newScore = scores.getOrDefault(p.getNguoiDung().getId(), 0);
//            int correct = correctMap.getOrDefault(p.getNguoiDung().getId(), p.getSoCauDung() != null ? p.getSoCauDung() : 0);
//            p.setDiem(newScore);
//            p.setSoCauDung(correct);
//        });
//
//        // Xếp hạng
//        players.sort(Comparator.comparing(NguoiChoiTranDau::getDiem).reversed());
//        AtomicInteger rank = new AtomicInteger(1);
//        players.forEach(p -> p.setXepHang(rank.getAndIncrement()));
//        nguoiChoiTranDauRepository.saveAll(players);
//
//        // 6️⃣ Cập nhật trạng thái trận
//        td.setTrangThai(TrangThaiTranDau.FINISHED);
//        td.setKetThucLuc(LocalDateTime.now());
//        tranDauRepository.save(td);
//
//        // 7️⃣ Lưu lịch sử nhanh (batch insert)
//        List<LichSuTranDau> lichSuList = players.stream().map(p -> LichSuTranDau.builder()
//                .tranDau(td)
//                .nguoiDung(p.getNguoiDung())
//                .tongDiem(p.getDiem())
//                .tongCauDung(p.getSoCauDung())
//                .xepHang(p.getXepHang())
//                .hoanThanhLuc(LocalDateTime.now())
//                .build()).toList();
//        lichSuTranDauRepository.saveAll(lichSuList);
//
//        // 8️⃣ Tìm người thắng
//        NguoiChoiTranDau winner = players.getFirst();
//        FinishedEvent.Winner winData = FinishedEvent.Winner.builder()
//                .userId(winner.getNguoiDung().getId())
//                .hoTen(winner.getNguoiDung().getHoTen())
//                .diem(winner.getDiem())
//                .tongCauDung(winner.getSoCauDung())
//                .build();
//
//        // 9️⃣ Phát WS FINISHED event
//        wsPublisher.publishFinished(
//                td.getId(),
//                td.getTenPhong(),
//                td.getMaPhong(),
//                td.getBatDauLuc(),
//                td.getKetThucLuc(),
//                winData,
//                players.stream().map(p -> FinishedEvent.Player.builder()
//                        .userId(p.getNguoiDung().getId())
//                        .hoTen(p.getNguoiDung().getHoTen())
//                        .diem(p.getDiem())
//                        .tongCauDung(p.getSoCauDung())
//                        .xepHang(p.getXepHang())
//                        .build()).toList()
//        );
//
//        // 🔟 Xóa state khỏi RAM
//        battleStateManager.remove(tranDauId);
//
//        // 🔁 Trả response
//        Map<Long, Integer> scoreMap = players.stream().collect(Collectors.toMap(
//                p -> p.getNguoiDung().getId(), NguoiChoiTranDau::getDiem));
//        return BattleFinishResponse.from(td, scoreMap, players.stream().map(NguoiChoiTranDau::getNguoiDung).toList());
//    }
//
//

//    @Override
//    @Transactional
//    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
//        // 1️⃣ Lấy trận đấu
//        TranDau td = tranDauRepository.findById(tranDauId)
//                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
//
//        // 2️⃣ Kiểm tra quyền (nếu không phải auto thì chỉ chủ phòng mới được kết thúc)
//        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId)) {
//            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
//        }
//
//        // Nếu đã FINISHED rồi thì trả lại kết quả cũ (tránh double-finish)
//        if (TrangThaiTranDau.FINISHED.equals(td.getTrangThai())) {
//            // Lấy lại điểm + người chơi từ DB để build response
//            List<NguoiChoiTranDau> playersDb = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId());
//            Map<Long, Integer> scoreMapDb = playersDb.stream()
//                    .collect(Collectors.toMap(
//                            p -> p.getNguoiDung().getId(),
//                            p -> p.getDiem() != null ? p.getDiem() : 0
//                    ));
//            List<NguoiDung> usersDb = playersDb.stream()
//                    .map(NguoiChoiTranDau::getNguoiDung)
//                    .toList();
//            return BattleFinishResponse.from(td, scoreMapDb, usersDb);
//        }
//
//        // 3️⃣ Lấy BattleState trong RAM (nếu có)
//        BattleState state = battleStateManager.get(tranDauId);
//
//        // Chặn double-finish trên state
//        if (state != null && !state.markFinishedOnce()) {
//            Map<Long, Integer> s = new HashMap<>(state.getDiemNguoiChoi());
//            List<NguoiDung> users = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId())
//                    .stream().map(NguoiChoiTranDau::getNguoiDung).toList();
//            return BattleFinishResponse.from(td, s, users);
//        }
//
//        // 4️⃣ Lấy danh sách người chơi trong trận từ DB
//        List<NguoiChoiTranDau> players = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId());
//
//        // 5️⃣ Chuẩn bị map điểm
//        Map<Long, Integer> scoreMap = new HashMap<>();
//        if (state != null && state.getDiemNguoiChoi() != null && !state.getDiemNguoiChoi().isEmpty()) {
//            scoreMap.putAll(state.getDiemNguoiChoi());
//        } else {
//            // Fallback: lấy từ DB (trường hợp server restart giữa trận)
//            for (NguoiChoiTranDau p : players) {
//                Long uid = p.getNguoiDung().getId();
//                scoreMap.put(uid, p.getDiem() != null ? p.getDiem() : 0);
//            }
//        }
//
//        // 6️⃣ Tính số câu đúng cho từng user (nếu còn BattleState)
//        Map<Long, Integer> correctMap = new HashMap<>();
//        if (state != null && state.getAnswers() != null && state.getDanhSachCauHoi() != null) {
//            // answers: questionIndex -> (userId -> answer)
//            for (Map.Entry<Integer, ConcurrentHashMap<Long, String>> entry : state.getAnswers().entrySet()) {
//                Integer questionIndex = entry.getKey();
//                if (questionIndex == null) continue;
//
//                // Lấy câu hỏi theo index
//                if (questionIndex < 0 || questionIndex >= state.getDanhSachCauHoi().size()) continue;
//                CauHoi cauHoi = state.getDanhSachCauHoi().get(questionIndex);
//                if (cauHoi == null || cauHoi.getDapAnDung() == null) continue;
//
//                char correctChar = Character.toUpperCase(cauHoi.getDapAnDung());
//
//                // Duyệt từng user đã trả lời câu này
//                for (Map.Entry<Long, String> ansEntry : entry.getValue().entrySet()) {
//                    Long uid = ansEntry.getKey();
//                    String ans = ansEntry.getValue();
//                    if (uid == null || ans == null) continue;
//
//                    if (ans.trim().equalsIgnoreCase(String.valueOf(correctChar))) {
//                        correctMap.merge(uid, 1, Integer::sum);
//                    }
//                }
//            }
//        }
//
//        // 7️⃣ Cập nhật điểm + số câu đúng lên bảng nguoi_choi_tran_dau
//        for (NguoiChoiTranDau p : players) {
//            Long uid = p.getNguoiDung().getId();
//
//            int newScore = scoreMap.getOrDefault(uid, p.getDiem() != null ? p.getDiem() : 0);
//            int correct = correctMap.getOrDefault(uid, p.getSoCauDung() != null ? p.getSoCauDung() : 0);
//
//            p.setDiem(newScore);
//            p.setSoCauDung(correct);
//        }
//
//        // 8️⃣ Xếp hạng theo điểm giảm dần
//        players.sort(Comparator.comparing(NguoiChoiTranDau::getDiem, Comparator.nullsFirst(Integer::compareTo)).reversed());
//        AtomicInteger rankCounter = new AtomicInteger(1);
//        players.forEach(p -> p.setXepHang(rankCounter.getAndIncrement()));
//
//        // Lưu lại vào DB
//        nguoiChoiTranDauRepository.saveAll(players);
//
//        // 9️⃣ Cập nhật trạng thái trận đấu
//        td.setTrangThai(TrangThaiTranDau.FINISHED);
//        td.setKetThucLuc(LocalDateTime.now());
//        tranDauRepository.save(td);
//
//        // 🔟 Lưu lịch sử trận đấu cho từng người chơi
//        List<LichSuTranDau> lichSuList = players.stream().map(p ->
//                LichSuTranDau.builder()
//                        .tranDau(td)
//                        .nguoiDung(p.getNguoiDung())
//                        .tongDiem(p.getDiem())
//                        .soCauDung(p.getSoCauDung())
//                        .tongThoiGianMs(0)         // <--- BẮT BUỘC
//                        .xepHang(p.getXepHang())   // <--- BẮT BUỘC PHẢI CÓ GIÁ TRỊ
//                        .hoanThanhLuc(LocalDateTime.now())
//                        .build()
//        ).toList();
//        lichSuTranDauRepository.saveAll(lichSuList);
//
//        // 1️⃣1️⃣ Xác định người thắng
//        FinishedEvent.Winner winnerData = null;
//        if (!players.isEmpty()) {
//            NguoiChoiTranDau winner = players.get(0);
//            winnerData = FinishedEvent.Winner.builder()
//                    .userId(winner.getNguoiDung().getId())
//                    .hoTen(winner.getNguoiDung().getHoTen())
//                    .diem(winner.getDiem() != null ? winner.getDiem() : 0)
//                    .soCauDung(winner.getSoCauDung() != null ? winner.getSoCauDung() : 0)
//                    .build();
//        }
//
//        // 1️⃣2️⃣ Chuẩn bị leaderboard để gửi qua WS
//        List<FinishedEvent.Player> leaderboard = players.stream()
//                .map(p -> FinishedEvent.Player.builder()
//                        .userId(p.getNguoiDung().getId())
//                        .hoTen(p.getNguoiDung().getHoTen())
//                        .diem(p.getDiem() != null ? p.getDiem() : 0)
//                        .soCauDung(p.getSoCauDung() != null ? p.getSoCauDung() : 0)
//                        .xepHang(p.getXepHang() != null ? p.getXepHang() : 0)
//                        .build())
//                .toList();
//
//        // 1️⃣3️⃣ Phát event FINISHED qua WebSocket cho tất cả client trong phòng
//        wsPublisher.publishFinished(
//                td.getId(),
//                td.getTenPhong(),
//                td.getMaPhong(),
//                td.getBatDauLuc(),
//                td.getKetThucLuc(),
//                winnerData,
//                leaderboard
//        );
//
//        // 1️⃣4️⃣ Xóa BattleState khỏi RAM
//        battleStateManager.remove(tranDauId);
//
//        // 1️⃣5️⃣ Build response REST cho FE (nếu FE có call /finish)
//        Map<Long, Integer> responseScoreMap = players.stream()
//                .collect(Collectors.toMap(
//                        p -> p.getNguoiDung().getId(),
//                        p -> p.getDiem() != null ? p.getDiem() : 0
//                ));
//        List<NguoiDung> responseUsers = players.stream()
//                .map(NguoiChoiTranDau::getNguoiDung)
//                .toList();
//
//        return BattleFinishResponse.from(td, responseScoreMap, responseUsers);
//    }

//    @Override
//    @Transactional
//    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
//        // 1️⃣ Lấy trận đấu
//        TranDau td = tranDauRepository.findById(tranDauId)
//                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
//
//        // 2️⃣ Quyền hạn: chỉ chủ phòng (trừ khi autoMode = true)
//        if (!autoMode && !Objects.equals(td.getChuPhong().getId(), currentUserId)) {
//            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
//        }
//
//        // 3️⃣ Nếu đã FINISHED rồi -> đọc từ DB & trả luôn (idempotent)
//        if (TrangThaiTranDau.FINISHED.equals(td.getTrangThai())) {
//            return buildFinishResponseFromDb(td);
//        }
//
//        // 4️⃣ Chỉ xử lý kết thúc khi đang ONGOING
//        if (!TrangThaiTranDau.ONGOING.equals(td.getTrangThai())) {
//            // Ví dụ PENDING hoặc trạng thái lạ -> trả thông tin hiện tại
//            return buildFinishResponseFromDb(td);
//        }
//
//        // 5️⃣ Lấy danh sách người chơi trong trận
//        List<NguoiChoiTranDau> players = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId());
//
//        if (players.isEmpty()) {
//            // Không có người chơi nhưng vẫn kết thúc trận
//            td.setTrangThai(TrangThaiTranDau.FINISHED);
//            td.setKetThucLuc(LocalDateTime.now());
//            tranDauRepository.save(td);
//            battleStateManager.remove(tranDauId);
//
//            return BattleFinishResponse.from(td, Collections.emptyMap(), Collections.emptyList());
//        }
//
//        // 6️⃣ Map điểm: ưu tiên BattleState, thiếu thì lấy từ DB
//        Map<Long, Integer> scoreMap = new HashMap<>();
//
//        BattleState state = battleStateManager.get(tranDauId);
//        if (state != null && state.getDiemNguoiChoi() != null && !state.getDiemNguoiChoi().isEmpty()) {
//            scoreMap.putAll(state.getDiemNguoiChoi());
//        }
//
//        // Fallback từ nguoi_choi_tran_dau
//        for (NguoiChoiTranDau p : players) {
//            Long uid = p.getNguoiDung().getId();
//            scoreMap.putIfAbsent(uid, Optional.ofNullable(p.getDiem()).orElse(0));
//        }
//
//        // 7️⃣ Thống kê log trả lời (số câu đúng + tổng thời gian)
//        List<TraLoiTranDau> logs = traLoiTranDauRepository.findAllByTranDau_Id(td.getId());
//        Map<Long, Integer> correctMap = new HashMap<>();
//        Map<Long, Integer> totalTimeMap = new HashMap<>();
//
//        for (TraLoiTranDau log : logs) {
//            Long uid = log.getNguoiDung().getId();
//
//            if (Boolean.TRUE.equals(log.getDungHaySai())) {
//                correctMap.merge(uid, 1, Integer::sum);
//            }
//            if (log.getThoiGianMs() != null) {
//                totalTimeMap.merge(uid, log.getThoiGianMs(), Integer::sum);
//            }
//        }
//
//        // 8️⃣ Cập nhật điểm + số câu đúng vào nguoi_choi_tran_dau
//        for (NguoiChoiTranDau p : players) {
//            Long uid = p.getNguoiDung().getId();
//            p.setDiem(scoreMap.getOrDefault(uid, 0));
//            p.setSoCauDung(correctMap.getOrDefault(uid, 0));
//        }
//
//        // 9️⃣ Xếp hạng theo điểm giảm dần
//        players.sort(Comparator.comparing(NguoiChoiTranDau::getDiem).reversed());
//        AtomicInteger rankCounter = new AtomicInteger(1);
//        players.forEach(p -> p.setXepHang(rankCounter.getAndIncrement()));
//        nguoiChoiTranDauRepository.saveAll(players);
//
//        // 🔟 Cập nhật winner + trạng thái trận
//        NguoiChoiTranDau winnerPlayer = players.get(0);
//
//        td.setWinner(winnerPlayer.getNguoiDung());
//        td.setTrangThai(TrangThaiTranDau.FINISHED);
//        td.setKetThucLuc(LocalDateTime.now());
//        tranDauRepository.save(td);
//
//        // 1️⃣1️⃣ Lưu lịch sử trận đấu
//        // Xóa lịch sử cũ để tránh trùng, nếu có gọi lại finish nhiều lần
//        lichSuTranDauRepository.deleteByTranDau_Id(td.getId());
//
//        LocalDateTime now = LocalDateTime.now();
//        List<LichSuTranDau> lichSuList = players.stream()
//                .map(p -> {
//                    Long uid = p.getNguoiDung().getId();
//                    Integer tongTime = totalTimeMap.getOrDefault(uid, 0);
//                    return LichSuTranDau.builder()
//                            .tranDau(td)
//                            .nguoiDung(p.getNguoiDung())
//                            .tongDiem(p.getDiem())
//                            .soCauDung(p.getSoCauDung())
//                            .tongThoiGianMs(tongTime)
//                            .xepHang(p.getXepHang())
//                            .hoanThanhLuc(now)
//                            .build();
//                })
//                .toList();
//
//        lichSuTranDauRepository.saveAll(lichSuList);
//
//        // 1️⃣2️⃣ Bắn WS FINISHED
//        FinishedEvent.Winner winData = FinishedEvent.Winner.builder()
//                .userId(winnerPlayer.getNguoiDung().getId())
//                .hoTen(winnerPlayer.getNguoiDung().getHoTen())
//                .diem(winnerPlayer.getDiem())
//                .soCauDung(winnerPlayer.getSoCauDung())
//                .build();
//
//        System.out.println("🔥 [FINISH] Chuẩn bị publish FINISHED WS cho tran_dau_id = " + td.getId()
//                + ", so_nguoi_choi = " + players.size());
//
//        wsPublisher.publishFinished(
//                td.getId(),
//                td.getTenPhong(),
//                td.getMaPhong(),
//                td.getBatDauLuc(),
//                td.getKetThucLuc(),
//                winData,
//                players.stream()
//                        .map(p -> FinishedEvent.Player.builder()
//                                .userId(p.getNguoiDung().getId())
//                                .hoTen(p.getNguoiDung().getHoTen())
//                                .diem(p.getDiem())
//                                .soCauDung(p.getSoCauDung())
//                                .xepHang(p.getXepHang())
//                                .build())
//                        .toList()
//        );
//
//        // 1️⃣3️⃣ Dọn state trong RAM
//        battleStateManager.remove(tranDauId);
//
//        // 1️⃣4️⃣ Build response REST
//        Map<Long, Integer> finalScores = players.stream()
//                .collect(Collectors.toMap(
//                        p -> p.getNguoiDung().getId(),
//                        NguoiChoiTranDau::getDiem
//                ));
//
//        List<NguoiDung> allUsers = players.stream()
//                .map(NguoiChoiTranDau::getNguoiDung)
//                .toList();
//
//        return BattleFinishResponse.from(td, finalScores, allUsers);
//    }
//
//    private BattleFinishResponse buildFinishResponseFromDb(TranDau td) {
//        List<NguoiChoiTranDau> players = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId());
//
//        Map<Long, Integer> scores = players.stream()
//                .collect(Collectors.toMap(
//                        p -> p.getNguoiDung().getId(),
//                        p -> Optional.ofNullable(p.getDiem()).orElse(0)
//                ));
//
//        List<NguoiDung> users = players.stream()
//                .map(NguoiChoiTranDau::getNguoiDung)
//                .toList();
//
//        return BattleFinishResponse.from(td, scores, users);
//    }

    @Override
    @Transactional
    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
        System.out.println(">>> [SERVICE] finishBattle CALLED, tranDauId=" + tranDauId
                + ", currentUserId=" + currentUserId + ", autoMode=" + autoMode);

        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        System.out.println(">>> [SERVICE] tran_dau.trang_thai = " + td.getTrangThai());

        // 1️⃣ Quyền hạn
        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId)) {
            System.out.println("❌ [SERVICE] finishBattle: currentUserId KHÔNG phải chủ phòng");
            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
        }

        // Đã kết thúc rồi → trả kết quả cũ, KHÔNG publish WS nữa
        if (TrangThaiTranDau.FINISHED.equals(td.getTrangThai())) {
            System.out.println("⚠️ [SERVICE] Trận đấu đã ở trạng thái FINISHED, trả BattleFinishResponse cũ");
            return BattleFinishResponse.from(td, null, null);
        }

        // 2️⃣ Lấy state trong RAM (nếu còn)
        BattleState state = battleStateManager.get(tranDauId);
        if (state != null && !state.markFinishedOnce()) {
            // Có người khác finish trước rồi
            System.out.println("⚠️ [SERVICE] markFinishedOnce = false, có luồng khác đã finish trước");
            return BattleFinishResponse.from(td, state.getDiemNguoiChoi(), null);
        }

        // 3️⃣ Lấy danh sách người chơi trong phòng
        List<NguoiChoiTranDau> players = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId());
        System.out.println(">>> [SERVICE] So nguoi_choi_tran_dau = " + players.size());

        if (players.isEmpty()) {
            // Không có người chơi → chỉ đánh dấu FINISHED
            System.out.println("⚠️ [SERVICE] Không có người chơi nào, chỉ set FINISHED và return");
            td.setTrangThai(TrangThaiTranDau.FINISHED);
            td.setKetThucLuc(Instant.now());
            tranDauRepository.save(td);
            battleStateManager.remove(tranDauId);
            return BattleFinishResponse.from(td, null, null);
        }

        // 4️⃣ Map điểm (ưu tiên BattleState)
        Map<Long, Integer> scoreMap = new HashMap<>();
        if (state != null && state.getDiemNguoiChoi() != null && !state.getDiemNguoiChoi().isEmpty()) {
            scoreMap.putAll(state.getDiemNguoiChoi());
            System.out.println(">>> [SERVICE] scoreMap lấy từ BattleState size = " + scoreMap.size());
        } else {
            // fallback: từ bảng nguoi_choi_tran_dau
            for (NguoiChoiTranDau p : players) {
                scoreMap.put(
                        p.getNguoiDung().getId(),
                        p.getDiem() != null ? p.getDiem() : 0
                );
            }
            System.out.println(">>> [SERVICE] scoreMap fallback từ nguoi_choi_tran_dau size = " + scoreMap.size());
        }

        // 5️⃣ Lấy log trả lời để tính số câu đúng + tổng thời gian
        List<TraLoiTranDau> logs = traLoiTranDauRepository.findAllByTranDau_Id(td.getId());
        System.out.println(">>> [SERVICE] So tra_loi_tran_dau = " + logs.size());

        Map<Long, Integer> correctMap = new HashMap<>();
        Map<Long, Integer> totalTimeMap = new HashMap<>();

        for (TraLoiTranDau log : logs) {
            Long uid = log.getNguoiDung().getId();

            if (Boolean.TRUE.equals(log.getDungHaySai())) {
                correctMap.merge(uid, 1, Integer::sum);
            }
            if (log.getThoiGianMs() != null) {
                totalTimeMap.merge(uid, log.getThoiGianMs(), Integer::sum);
            }
        }

        // 6️⃣ Cập nhật điểm & số câu đúng vào nguoi_choi_tran_dau
        for (NguoiChoiTranDau p : players) {
            Long uid = p.getNguoiDung().getId();
            int diem = scoreMap.getOrDefault(uid, 0);
            int soCauDung = correctMap.getOrDefault(uid, 0);

            p.setDiem(diem);
            p.setSoCauDung(soCauDung);
        }

        // Xếp hạng theo điểm giảm dần
        players.sort(Comparator.comparing(NguoiChoiTranDau::getDiem).reversed());
        AtomicInteger rankCounter = new AtomicInteger(1);
        players.forEach(p -> p.setXepHang(rankCounter.getAndIncrement()));
        nguoiChoiTranDauRepository.saveAll(players);
        System.out.println(">>> [SERVICE] Đã cập nhật diem/so_cau_dung/xep_hang cho nguoi_choi_tran_dau");

        // 7️⃣ Cập nhật winner + trạng thái trận
        NguoiChoiTranDau winnerPlayer = players.get(0);
        td.setWinner(winnerPlayer.getNguoiDung());        // => sẽ update winner_id
        td.setTrangThai(TrangThaiTranDau.FINISHED);
        td.setKetThucLuc(Instant.now());
        tranDauRepository.save(td);
        System.out.println(">>> [SERVICE] Winner = " + winnerPlayer.getNguoiDung().getHoTen()
                + ", diem = " + winnerPlayer.getDiem());

        // 8️⃣ Lưu lịch sử trận đấu
        Instant now = Instant.now();
        List<LichSuTranDau> lichSuList = players.stream()
                .map(p -> {
                    Long uid = p.getNguoiDung().getId();
                    Integer tongTime = totalTimeMap.getOrDefault(uid, 0);
                    return LichSuTranDau.builder()
                            .tranDau(td)
                            .nguoiDung(p.getNguoiDung())
                            .tongDiem(p.getDiem())
                            .soCauDung(p.getSoCauDung())
                            .tongThoiGianMs(tongTime)
                            .xepHang(p.getXepHang())
                            .hoanThanhLuc(now)
                            .build();
                })
                .toList();

        lichSuTranDauRepository.saveAll(lichSuList);
        System.out.println(">>> [SERVICE] Đã lưu lich_su_tran_dau, size=" + lichSuList.size());

        // 9️⃣ Phát WS FINISHED event
        FinishedEvent.Winner winData = FinishedEvent.Winner.builder()
                .userId(winnerPlayer.getNguoiDung().getId())
                .hoTen(winnerPlayer.getNguoiDung().getHoTen())
                .diem(winnerPlayer.getDiem())
                .soCauDung(winnerPlayer.getSoCauDung())
                .build();

        System.out.println("🔥 [SERVICE] Chuẩn bị publish FINISHED WS cho tran_dau_id = " + td.getId()
                + ", so_nguoi_choi = " + players.size());

        wsPublisher.publishFinished(
                td.getId(),
                td.getTenPhong(),
                td.getMaPhong(),
                td.getBatDauLuc(),
                td.getKetThucLuc(),
                winData,
                players.stream()
                        .map(p -> FinishedEvent.Player.builder()
                                .userId(p.getNguoiDung().getId())
                                .hoTen(p.getNguoiDung().getHoTen())
                                .diem(p.getDiem())
                                .soCauDung(p.getSoCauDung())
                                .xepHang(p.getXepHang())
                                .build())
                        .toList()
        );

        // 🔟 Dọn state trong RAM
        battleStateManager.remove(tranDauId);
        System.out.println(">>> [SERVICE] Đã remove BattleState khỏi RAM");

        // 🔁 Build response REST
        Map<Long, Integer> finalScores = players.stream()
                .collect(Collectors.toMap(
                        p -> p.getNguoiDung().getId(),
                        NguoiChoiTranDau::getDiem
                ));

        List<NguoiDung> allUsers = players.stream()
                .map(NguoiChoiTranDau::getNguoiDung)
                .toList();

        return BattleFinishResponse.from(td, finalScores, allUsers);
    }


    @Transactional(readOnly = true)
    @Override
    public SyncStateResponse syncState(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        BattleState state = battleStateManager.get(tranDauId);
        int seconds = td.getGioiHanThoiGianCauGiay() != null
                ? td.getGioiHanThoiGianCauGiay()
                : 15;

        // ❌ Sai: !"ONGOING".equals(td.getTrangThai())
        // ✅ Đúng:
        if (state == null || !TrangThaiTranDau.ONGOING.equals(td.getTrangThai())) {
            return SyncStateResponse.builder()
                    .tranDauId(td.getId())
                    .trangThai(td.getTrangThai())
                    .secondsPerQuestion(seconds)
                    .currentQuestionIndex(-1)
                    .myTotalPoints(0)
                    .build();
        }

        int idx = state.getCurrentQuestionIndex();
        CauHoi q = (idx >= 0 && idx < state.getDanhSachCauHoi().size())
                ? state.getDanhSachCauHoi().get(idx)
                : null;

        return SyncStateResponse.builder()
                .tranDauId(td.getId())
                .trangThai(td.getTrangThai())
                .currentQuestionIndex(idx)
                .currentQuestionStart(state.getCurrentQuestionStart())
                .secondsPerQuestion(seconds)
                .currentQuestionId(q != null ? q.getId() : null)
                .noiDung(q != null ? q.getNoiDung() : null)
                .loaiNoiDung(q != null ? q.getLoaiNoiDung() : null)
                .duongDanTep(q != null ? q.getDuongDanTep() : null)
                .a(q != null ? q.getLuaChonA() : null)
                .b(q != null ? q.getLuaChonB() : null)
                .c(q != null ? q.getLuaChonC() : null)
                .d(q != null ? q.getLuaChonD() : null)
                .myTotalPoints(currentUserId != null
                        ? state.getDiemNguoiChoi().getOrDefault(currentUserId, 0)
                        : 0)
                .build();
    }

    @Override
    public Page<LichSuTranDauResponse> getMyHistory(Long currentUserId, int page, int limit) {
        PageRequest pageable = PageRequest.of(page, limit);
        return lichSuTranDauRepository
                .findByNguoiDung_IdOrderByHoanThanhLucDesc(currentUserId, pageable)
                .map(LichSuTranDauResponse::fromEntity);
    }


    @Override
    public LichSuTranDauDetailResponse getMyHistoryDetail(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        LichSuTranDau myHistory = lichSuTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDauId, currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa tham gia trận đấu này"));

        // base info
        LichSuTranDauDetailResponse res = LichSuTranDauDetailResponse.baseFrom(td, myHistory);

        // leaderboard
        List<LichSuTranDau> all = lichSuTranDauRepository
                .findByTranDau_IdOrderByXepHangAsc(tranDauId);

        List<FinishedPlayer> leaderboard = all.stream()
                .map(ls -> FinishedPlayer.builder()
                        .userId(ls.getNguoiDung().getId())
                        .hoTen(ls.getNguoiDung().getHoTen())
                        .diem(ls.getTongDiem())
                        .soCauDung(ls.getSoCauDung())
                        .xepHang(ls.getXepHang())
                        .build())
                .toList();

        res.setLeaderboard(leaderboard);

        // câu hỏi / đáp án của riêng user
        List<TraLoiTranDau> answers = traLoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_IdOrderByTraLoiLucAsc(tranDauId, currentUserId);

        List<LichSuTranDauQuestionResponse> qList = answers.stream()
                .map(tl -> LichSuTranDauQuestionResponse.fromEntities(
                        tl,
                        tl.getCauHoi()      // đã quan hệ @ManyToOne
                ))
                .toList();
        res.setQuestions(qList);
        return res;
    }

    @Override
    public void guiChatTrongTran(GuiChatDTO dto, Long currentUserId) throws Exception {
        NguoiDung nguoiDung = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        TranDau tranDau = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        // Không cho chat ở trận đã kết thúc (tuỳ bạn)
        if (tranDau.getTrangThai() == TrangThaiTranDau.FINISHED) {
            throw new IllegalStateException("Trận đấu đã kết thúc, không thể chat");
        }

        // Bắt buộc phải là người trong phòng
        boolean joined = nguoiChoiTranDauRepository
                .existsByTranDauIdAndNguoiDungId(tranDau.getId(), nguoiDung.getId());
        if (!joined) {
            throw new PermissionDenyException("Bạn chưa tham gia trận đấu này");
        }

        // Không lưu DB, chỉ broadcast WS
        wsPublisher.publishChatMessage(
                tranDau.getId(),
                nguoiDung.getId(),
                nguoiDung.getHoTen(),
                dto.getNoiDung(),
                false // system = false
        );
    }

    private void updateAndBroadcastLeaderboard(Long tranDauId, BattleState optionalState) {
        BattleState state = (optionalState != null)
                ? optionalState : battleStateManager.get(tranDauId);
        if (state == null) return;

        Map<Long, Integer> scores = state.getDiemNguoiChoi();
        if (scores.isEmpty()) return;

        List<Long> ids = new ArrayList<>(scores.keySet());
        Map<Long, String> nameMap = nguoiDungRepository.findAllById(ids).stream()
                .collect(Collectors.toMap(NguoiDung::getId, NguoiDung::getHoTen));

        AtomicInteger rank = new AtomicInteger(1);
        List<LeaderboardUpdateEvent.Row> board = scores.entrySet().stream()
                .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
                .map(e -> LeaderboardUpdateEvent.Row.builder()
                        .userId(e.getKey())
                        .hoTen(nameMap.getOrDefault(e.getKey(), "Người chơi"))
                        .diem(e.getValue())
                        .xepHang(rank.getAndIncrement()).build())
                .toList();

        wsPublisher.publishLeaderboard(tranDauId, board);
    }


    @Transactional
    public void updateAndBroadcastLeaderboard(Long tranDauId) {
        List<NguoiChoiTranDau> all = nguoiChoiTranDauRepository.findAllByTranDau_Id(tranDauId);

        // Sắp xếp: điểm giảm dần, cùng điểm thì ai vào trước xếp cao hơn
        all.sort(Comparator
                .comparing(NguoiChoiTranDau::getDiem, Comparator.nullsFirst(Comparator.naturalOrder())).reversed()
                .thenComparing(NguoiChoiTranDau::getThamGiaLuc, Comparator.nullsFirst(Comparator.naturalOrder())));

        int rank = 1;
        for (NguoiChoiTranDau p : all) p.setXepHang(rank++);
        nguoiChoiTranDauRepository.saveAll(all);

        var rows = all.stream().map(p -> LeaderboardUpdateEvent.Row.builder()
                .userId(p.getNguoiDung().getId())
                .hoTen(p.getNguoiDung().getHoTen())
                .diem(Optional.ofNullable(p.getDiem()).orElse(0))
                .soCauDung(Optional.ofNullable(p.getSoCauDung()).orElse(0))
                .xepHang(Optional.ofNullable(p.getXepHang()).orElse(0))
                .build()).toList();

        wsPublisher.publishLeaderboard(tranDauId, rows);
    }

}
